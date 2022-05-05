
type token = int

let (gen_token, reset_token_gen) =
    let seed = ref 0 in
    ( (fun () -> incr seed; !seed)
    , (fun () -> seed := 0) )

type root =
    | Var of string
    | Tok of token


type selector =
    | Field of int
    | Deref

type access =
    { root : root
    ; path : selector list }

let access_of_var   var = { root = Var var; path = [] }
let access_of_token tok = { root = Tok tok; path = [] }


type access_relation =
    | Eq
    | Irr
    | Lt of selector list
    | Gt of selector list

let rec compare_path p1 p2 =
    match p1, p2 with
    | []       , []        -> Eq
    | []       , p2        -> Gt p2
    | p1       , []        -> Lt p1
    | s1 :: p1', s2 :: p2' ->
        if s1 = s2
        then compare_path p1' p2'
        else Irr

let compare_access a1 a2 =
    if a1.root <> a2.root
    then Irr
    else compare_path a1.path a2.path



type mutability = Imm | Mut

let min_mutability m1 m2 =
    match m1, m2 with
    | Mut, Mut -> Mut
    | _  , _   -> Imm



type borrowers =
    { imm : (selector list * token) list
    ; mut : (selector list * token) list }

module RootMap = Map.Make(struct
        type t = root
        let compare = Stdlib.compare
    end)

type constr_set = borrowers RootMap.t

let empty_cset = RootMap.empty


type constr =
    { borrower : token
    ; kind     : mutability
    ; borrowee : access }

let add_constr_to_borrows constr borrows =
    match constr.kind with
    | Imm ->
        { borrows with imm = (constr.borrowee.path, constr.borrower)
                            :: borrows.imm }
    | Mut ->
        { borrows with mut = (constr.borrowee.path, constr.borrower)
                            :: borrows.mut }

let add_constr constr cset =
    cset |> RootMap.update constr.borrowee.root @@ function
    | None         -> Some (add_constr_to_borrows constr {imm=[]; mut=[]})
    | Some borrows -> Some (add_constr_to_borrows constr borrows)


let split_borrows (path, kind) borrows =
    let split = List.partition @@ fun (path', borrower) ->
        match compare_path path path' with
        | Irr -> false
        | _   -> true
    in
    let conflicting_imm, imm' =
        match kind with Imm -> ([], borrows.imm)
                      | Mut -> split borrows.imm
    in
    let conflicting_mut, mut' = split borrows.mut in
    ( List.rev_map snd (List.rev_append conflicting_mut conflicting_imm)
    , { imm = imm'; mut = mut' } )


let split_cset (acc, kind) cset =
    let rec loop (acc, kind) (conflicting, cset) =
        let delta = ref [] in
        let cset' = cset |> RootMap.update acc.root @@ function
            | None -> None
            | Some borrows ->
                let cf, borrows' = split_borrows (acc.path, kind) borrows in
                delta := List.filter
                        (fun tok -> not @@ List.mem tok conflicting)
                        cf;
                match borrows' with
                | {imm=[]; mut=[]} -> None
                | _                -> Some borrows'
        in
        List.fold_left begin fun (conflicting, cset) tok ->
            loop (access_of_token tok, Mut) (conflicting, cset)
        end (List.rev_append !delta conflicting, cset') !delta
    in
    loop (acc, kind) ([], cset)


let merge_borrows brs1 brs2 =
    let merge_list l1 l2 =
        l1
        |> List.filter (fun elem -> not @@ List.mem elem l2)
        |> Fun.flip List.rev_append l2
    in
    { imm = merge_list brs1.imm brs2.imm
    ; mut = merge_list brs1.mut brs2.mut }

let merge_cset cset1 cset2 =
    RootMap.merge begin fun _ brs1 brs2 ->
        match brs1, brs2 with
        | Some brs1, Some brs2 -> Some(merge_borrows brs1 brs2)
        | Some brs , None
        | None     , Some brs  -> Some brs
        | None     , None      -> None
    end cset1 cset2
