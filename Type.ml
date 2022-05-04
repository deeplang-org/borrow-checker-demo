
module A = Access

type permission = POwner | PBorrow of Access.mutability

type typ =
    | Int
    | Tuple  of typ list
    | IBorrow of A.token * typ
    | MBorrow of A.token * typ * typ

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

let rec read_path_aux path (typ_r, typ_w) =
    match path, typ_r, typ_w with
    | [], _, _ ->
        (POwner, typ_r, typ_w)
    | A.Field k :: path', Tuple typs_r, Tuple typs_w ->
        read_path_aux path' (List.nth typs_r k, List.nth typs_w k)
    | A.Deref :: path', IBorrow(_, typ'), _ ->
        let (perm, typ_r', typ_w') = read_path_aux path' (typ', typ') in
        ( min_permission perm (PBorrow Imm), typ_r', typ_w' )
    | A.Deref :: path', MBorrow(_, typ_r, typ_w), _ ->
        let (perm, typ_r', typ_w') = read_path_aux path' (typ_r, typ_w) in
        ( min_permission perm (PBorrow Mut), typ_r', typ_w' )
    | _ ->
        raise(InvalidPath(path, typ_r))

let read_path path typ =
    read_path_aux path (typ, typ)


let rec write_path_aux f perm path typ =
    match path, typ with
    | [], _ ->
        f perm typ
    | A.Field k :: path', Tuple typs ->
        Tuple( typs |> List.mapi @@ fun k' typ' ->
            if k = k'
            then write_path_aux f perm path' typ'
            else typ' )
    | A.Deref :: path', IBorrow(tok, typ') ->
        IBorrow(tok, write_path_aux f (PBorrow Imm) path' typ')
    | A.Deref :: path', MBorrow(tok, typ_r, typ_w) ->
        MBorrow(tok, typ_r, write_path_aux f
                (min_permission perm (PBorrow Mut)) path' typ_w)
    | _ ->
        raise(InvalidPath(path, typ))

let write_path f path typ = write_path_aux f POwner path typ



exception TypeMismatch of typ * typ

let relate_access (mode : [`Sub | `Shape]) mut cset a_sub a_sup =
    let open A in
    match mode with
    | `Sub ->
        cset
        |> add_constr { borrower = a_sup
                      ; kind     = mut
                      ; borrowee = access_of_token a_sub }
    | `Shape ->
        cset

let rec relate_typ (mode : [`Sub | `Shape]) cset sub sup =
    match sub, sup with
    | Int, Int ->
        cset
    | Tuple subs, Tuple sups when List.compare_lengths subs sups = 0 ->
        List.fold_left2 (relate_typ mode) cset subs sups
    | IBorrow(a_sub, sub')
    , IBorrow(a_sup, sup') ->
        relate_typ mode
            (relate_access mode Imm cset a_sub a_sup)
            sub' sup'
    | MBorrow(a_sub, sub_r, sub_w)
    , MBorrow(a_sup, sup_r, sup_w) ->
        let cset = relate_access mode Mut cset a_sub a_sup in
        let cset = relate_typ mode cset sub_r sup_r in
        relate_typ mode cset sup_w sub_w
    | _ ->
        raise(TypeMismatch(sub, sup))

let subtyp = relate_typ `Sub
let shape_eq typ1 typ2 =
    ignore (relate_typ `Shape A.empty_cset typ1 typ2)



let fresh_access (mode : [`Sub | `Sup]) cset mut a =
    let tok = A.gen_token() in
    let new_constr =
        match mode with
        | `Sub -> A.{ borrower = a
                    ; kind     = mut
                    ; borrowee = access_of_token tok }
        | `Sup -> A.{ borrower = tok
                    ; kind     = mut
                    ; borrowee = access_of_token a   }
    in
    ( tok, A.add_constr new_constr cset )

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
    | IBorrow(a, typ') ->
        let tok, cset = fresh_access mode cset A.Imm a in
        let typ', cset = fresh_typ mode cset typ' in
        ( IBorrow(tok, typ'), cset )
    | MBorrow(a, typ_r, typ_w) ->
        let tok, cset = fresh_access mode cset A.Imm a in
        let r_mode, w_mode =
            match mode with
            | `Sub -> `Sub, `Sup
            | `Sup -> `Sup, `Sub
        in
        let typ_r', cset = fresh_typ r_mode cset typ_r in
        let typ_w', cset = fresh_typ w_mode cset typ_w in
        ( MBorrow(tok, typ_r', typ_w'), cset )

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
    | IBorrow(tok, _)
    | MBorrow(tok, _, _) ->
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
        | Int                -> Int
        | Tuple typs         -> Tuple(List.map map_typ typs)
        | IBorrow(tok, typ') -> IBorrow(map_tok tok, map_typ typ')
        | MBorrow(tok, typ_r, typ_w) ->
            MBorrow(map_tok tok, map_typ typ_r, map_typ typ_w)
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
    | IBorrow(_, typ') ->
        fprintf fmt "&%a" pp_typ typ'
    | MBorrow( _, typ', _) ->
        fprintf fmt "&mut %a" pp_typ typ'
