
type path = string * Access.selector list

type expr =
    | Int    of int
    | Path   of Type.permission * path
    | Tuple  of expr list
    | Assign of path * expr
    | Let    of string * expr * expr
    | If     of expr * expr * expr


type operation =
    | OpMove   of path
    | OpBorrow of Access.mutability * path
    | OpAssign of path
    | OpScope  of string
    | OpReturn of string
    | OpResultLet

exception UnboundVariable  of string
exception InvalidPath      of path * Type.typ * Access.selector list
exception AccessDeadPath   of operation * operation
exception PermissionDenied of operation


let acc_readable acc grave =
    grave |> List.find_map @@ fun (acc', reason) ->
    match Access.compare_access acc acc' with
    | Irr -> None
    | _   -> Some reason

let acc_writable acc grave =
    grave |> List.find_map @@ fun (acc', reason) ->
    match Access.compare_access acc acc' with
    | Lt _ -> Some reason
    | _    -> None

let rec typ_is_alive grave typ =
    let open Type in
    match typ with
    | Int                  -> None
    | Tuple typs           -> List.find_map (typ_is_alive grave) typs
    | Borrow(_, tok, typ') ->
        match acc_readable (Access.access_of_token tok) grave with
        | Some reason -> Some reason
        | None        -> typ_is_alive grave typ'

let respawn_acc acc grave =
    grave |> List.filter @@ fun (acc', reason) ->
    match Access.compare_access acc' acc with
    | Eq | Lt _ -> false
    | _         -> true

let merge_grave grave1 grave2 =
    grave1
    |> List.filter (fun (acc, _) -> not @@ List.mem_assoc acc grave2)
    |> Fun.flip List.rev_append grave2


module Env = Map.Make(String)

let rec take_access ~op env kind acc (cset, grave) =
    let conflicting, cset = Access.split_cset (acc, kind) cset in
    let grave =
        conflicting
        |> List.rev_map (fun tok -> Access.access_of_token tok, op)
        |> Fun.flip List.rev_append grave
    in
    Env.to_seq env
    |> Seq.flat_map begin fun (var, typ) ->
        Type.owned_tokens typ
        |> Seq.map @@ fun (path, tok) ->
        ( Access.{ root = Var var; path = path }, tok )
    end
    |> Seq.filter
        (fun (acc, tok) -> List.mem tok conflicting
                           && acc_readable acc grave = None)
    |> Seq.map fst
    |> Seq.fold_left begin fun (cset, grave) acc ->
        let grave = (acc, op) :: grave in
        take_access ~op env Access.Mut acc (cset, grave)
    end (cset, grave)




let access_of_path (var, sels) =
    Access.{ root = Var var
           ; path = sels }



let rec check env (cset, grave) expr =
    match expr with
    | Int _ ->
        ( Type.Int, env, (cset, grave) )
    | Path(perm, ((var, sels) as path)) ->
        let op =
            match perm with
            | POwner    -> OpMove path
            | PBorrow m -> OpBorrow(m, path)
        in
        let acc = access_of_path path in
        begin match acc_readable acc grave with
        | Some reason -> raise(AccessDeadPath(op, reason))
        | None        -> ()
        end;

        let vtyp =
            match Env.find_opt var env with
            | Some typ -> typ
            | None     -> raise(UnboundVariable var)
        in
        let (allowed_perm, ptyp) =
            try Type.read_path sels vtyp with
              Type.InvalidPath(sels', typ') ->
                let l1 = List.length sels in
                let l2 = List.length sels' in
                let correct_sels =
                    List.filteri (fun i _ -> i + l2 < l1) sels
                in
                raise(InvalidPath((var, correct_sels), typ', sels'))
        in

        begin match perm, allowed_perm with
        | POwner     , PBorrow _
        | PBorrow Mut, PBorrow Imm -> raise(PermissionDenied op)
        | _                        -> ()
        end;
        let cset, grave = take_access ~op env
                (match perm with PBorrow Imm -> Imm | _ -> Mut)
                acc (cset, grave)
        in

        begin match perm with
        | POwner -> ( ptyp, env, (cset, (acc, op) :: grave) )
        | PBorrow mut ->
            let tok = Access.gen_token () in
            ( Type.Borrow(mut, tok, ptyp)
            , env
            , ( Access.add_constr
                    Access.{ borrower = tok
                           ; kind     = mut
                           ; borrowee = acc }
                    cset
              , grave ) )
        end
    | Tuple exprs ->
        let env = ref env in
        let ctx = ref (cset, grave) in
        let typs = exprs |> List.map @@ fun expr ->
            let typ, env', ctx' = check !env !ctx expr in
            ( env := env'; ctx := ctx'; typ )
        in
        ( Type.Tuple typs, !env, !ctx )
    | Assign((var, sels) as path, value) ->
        let vtyp, env, (cset, grave) = check env (cset, grave) value in
        let var_typ =
            match Env.find var env with
            | typ                 -> typ
            | exception Not_found -> raise(UnboundVariable var)
        in
        let acc = access_of_path path in
        let op = OpAssign path in
        let cset, grave = take_access ~op env Mut acc (cset, grave) in
        let csetr = ref cset in
        let graver = ref grave in
        let var_typ' =
            Type.write_path begin fun allowed_mut pacc ptyp ->
                match allowed_mut, pacc with
                | Imm, _      -> raise(PermissionDenied op)
                | _  , Some _ ->
                    csetr := Type.subtyp !csetr vtyp ptyp;
                    ptyp
                | _  , None   ->
                    Type.shape_eq ptyp vtyp;
                    graver := respawn_acc acc !graver;
                    vtyp
            end sels var_typ
        in
        ( Type.Int, Env.add var var_typ' env, (!csetr, !graver) )
    | Let("_", rhs, body) ->
        let _, env, (cset, grave) = check env (cset, grave) rhs in
        check env (cset, grave) body
    | Let(name, rhs, body) ->
        let rhs_typ, env, (cset, grave) = check env (cset, grave) rhs in
        let res_typ, env, (cset, grave) =
            check (Env.add name rhs_typ env) (cset, grave) body
        in
        let (cset, grave) = take_access ~op:(OpScope name)
                env Mut (Access.access_of_var name) (cset, grave)
        in
        begin match typ_is_alive grave res_typ with
        | Some reason -> raise(AccessDeadPath(OpResultLet, reason))
        | None        -> ()
        end;
        res_typ, (Env.remove name env), (cset, grave)
    | If(cond, conseq, alter) ->
        let cond_typ, env, (cset, grave) = check env (cset, grave) cond in
        Type.shape_eq cond_typ Type.Int;
        let conseq_typ, conseq_env, (conseq_cset, conseq_grave) =
            check env (cset, grave) conseq
        in
        let alter_typ, alter_env, (alter_cset, alter_grave) =
            check env (cset, grave) alter
        in
        let csetr = ref (Access.merge_cset conseq_cset alter_cset) in
        let env = Env.merge begin fun _ typ1 typ2 ->
                match typ1, typ2 with
                | Some typ1, Some typ2 ->
                    let typ, cset' = Type.least_suptyp !csetr
                            [typ1; typ2]
                    in
                    csetr := cset';
                    Some typ
                | Some typ , None
                | None     , Some typ  -> failwith "Expr.check"
                | None     , None      -> None
            end conseq_env alter_env
        in
        let ret_typ, cset = Type.least_suptyp !csetr
                [conseq_typ; alter_typ]
        in
        ( ret_typ, env, (cset, merge_grave conseq_grave alter_grave) )
