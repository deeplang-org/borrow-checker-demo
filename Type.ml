
module A = Access

type permission = POwner | PBorrow of Access.mutability

type typ =
    | Int
    | Tuple  of typ list
    | Borrow of A.mutability * A.token * typ

type func_typ =
    { args : typ list
    ; ret  : typ }




exception InvalidPath of A.selector list * typ

let min_permission pm1 pm2 =
    match pm1, pm2 with
    | POwner    , POwner     -> POwner
    | PBorrow m , POwner
    | POwner    , PBorrow m  -> PBorrow m
    | PBorrow m1, PBorrow m2 -> PBorrow(Access.min_mutability m1 m2)

let rec read_path path typ =
    match path, typ with
    | [], _ ->
        (POwner, typ)
    | A.Field k :: path', Tuple typs ->
        read_path path' (List.nth typs k)
    | A.Deref :: path', Borrow(mut, a, typ') ->
        let (perm, typ') = read_path path' typ' in
        ( min_permission perm (PBorrow mut)
        , typ' )
    | _ ->
        raise(InvalidPath(path, typ))


let rec write_path_aux f (allowed_mut, acc) path typ =
    match path, typ with
    | [], _ ->
        f allowed_mut acc typ
    | A.Field k :: path', Tuple typs ->
        Tuple( typs |> List.mapi @@ fun k' typ' ->
            if k = k'
            then write_path_aux f (allowed_mut, acc) path' typ'
            else typ' )
    | A.Deref :: path', Borrow(mut, tok, typ') ->
        Borrow( mut, tok
              , write_path_aux f
                  (A.min_mutability mut allowed_mut, Some tok)
                  path' typ' )

    | _ ->
        raise(InvalidPath(path, typ))

let write_path f path typ = write_path_aux f (A.Mut, None) path typ



exception TypeMismatch of typ * typ

let relate_access (mode : [`Sub | `Eqv | `Shape]) mut cset a_sub a_sup =
    let open A in
    match mode with
    | `Sub ->
        cset
        |> add_constr { borrower = a_sup
                      ; kind     = mut
                      ; borrowee = access_of_token a_sub }
    | `Eqv ->
        cset
        |> add_constr { borrower = a_sup
                      ; kind     = mut
                      ; borrowee = access_of_token a_sub }
        |> add_constr { borrower = a_sub
                      ; kind     = mut
                      ; borrowee = access_of_token a_sup }
    | `Shape ->
        cset

let rec relate_typ (mode : [`Sub | `Eqv | `Shape]) cset sub sup =
    match sub, sup with
    | Int, Int ->
        cset
    | Tuple subs, Tuple sups when List.compare_lengths subs sups = 0 ->
        List.fold_left2 (relate_typ mode) cset subs sups
    | Borrow(Imm, a_sub, sub')
    , Borrow(Imm, a_sup, sup') ->
        relate_typ mode
            (relate_access mode Imm cset a_sub a_sup)
            sub' sup'
    | Borrow(Mut, a_sub, sub')
    , Borrow(Mut, a_sup, sup') ->
        relate_typ (match mode with `Shape -> `Shape | _ -> `Eqv)
            (relate_access mode Mut cset a_sub a_sup)
            sub' sup'
    | _ ->
        raise(TypeMismatch(sub, sup))

let subtyp = relate_typ `Sub
let shape_eq typ1 typ2 =
    ignore (relate_typ `Eqv A.empty_cset typ1 typ2)



let rec fresh_typ (mode : [`Sub | `Sup]) cset typ =
    match typ with
    | Int ->
        ( Int , cset )
    | Tuple typs ->
        let (typs', cset') = List.fold_left
                (fun (typs, cset) typ ->
                            let typ', cset' = fresh_typ mode cset typ in
                            (typ' :: typs, cset'))
                ([], cset) typs
        in
        ( Tuple typs', cset' )
    | Borrow(mut, a, typ') ->
        let tok = A.gen_token () in
        let new_constr =
            match mode with
            | `Sub -> A.{ borrower = a
                        ; kind     = mut
                        ; borrowee = access_of_token tok }
            | `Sup -> A.{ borrower = tok
                        ; kind     = mut
                        ; borrowee = access_of_token a   }
        in
        let typ', cset' =
            match mut with
            | Imm -> fresh_typ mode cset typ'
            | Mut -> ( typ', cset )
        in
        ( Borrow(mut, tok, typ')
        , A.add_constr new_constr cset' )

let fresh_subtyp = fresh_typ `Sub
let fresh_suptyp = fresh_typ `Sup


let least_suptyp cset typs =
    match typs with
    | [] ->
        failwith "Type.least_suptyp"
    | typ :: typs ->
        let ret_typ, cset = fresh_suptyp cset typ in
        ( ret_typ
        , List.fold_left (fun cset typ -> subtyp cset typ ret_typ)
                cset typs )

let greatest_subtyp cset typs =
    match typs with
    | [] ->
        failwith "Type.greatest_subtyp"
    | typ :: typs ->
        let ret_typ, cset = fresh_subtyp cset typ in
        ( ret_typ
        , List.fold_left (fun cset typ -> subtyp cset ret_typ typ)
                cset typs )


let rec owned_tokens typ =
    match typ with
    | Int        -> Seq.empty
    | Tuple typs ->
        List.mapi (fun i typ -> i, owned_tokens typ) typs
        |> List.to_seq
        |> Seq.flat_map @@ fun (i, toks) ->
        Seq.map (fun (path, tok) -> (A.Field i :: path, tok)) toks
    | Borrow(borrow_mut, tok, typ') ->
        Seq.return ([], tok)


let instantiate_function func_typ =
    let tok_map = ref [] in
    let map_tok tok =
        match List.assoc tok !tok_map with
        | tok' ->
            tok'
        | exception _ ->
            let tok' = A.gen_token () in
            tok_map := (tok, tok') :: !tok_map;
            tok'
    in
    let rec map_typ = function
        | Int                    -> Int
        | Tuple typs             -> Tuple(List.map map_typ typs)
        | Borrow(mut, tok, typ') -> Borrow(mut, map_tok tok, map_typ typ')
    in
    { args = List.map map_typ func_typ.args
    ; ret  = map_typ func_typ.ret }




let rec pp_typ fmt typ =
    let open Format in
    match typ with
    | Int ->
        fprintf fmt "Int" 
    | Tuple typs ->
        fprintf fmt "(@[<hov2>%a@])"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
                            pp_typ)
            typs
    | Borrow(Imm, _, typ') ->
        fprintf fmt "&%a" pp_typ typ'
    | Borrow(Mut, _, typ') ->
        fprintf fmt "&mut %a" pp_typ typ'
