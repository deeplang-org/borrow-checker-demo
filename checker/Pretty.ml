


let rec string_of_path (mode : [`Big | `Small]) (var, sels) =
    match mode, sels with
    | mode, [] -> var
    | _, Access.Deref :: sels' ->
        string_of_path `Big ("*" ^ var, sels')
    | `Big, Access.Field k :: sels' ->
        string_of_path `Small ("(" ^ var ^ ")." ^ string_of_int k, sels')
    | `Small, Access.Field k :: sels' ->
        string_of_path `Small (var ^ "." ^ string_of_int k, sels')

let rec pp_path fmt (var, sels) =
    Format.fprintf fmt "%s" (string_of_path `Small (var, sels))



let rec pp_typ fmt typ =
    let open Type in
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




let pp_op fmt op =
    let open Expr in
    let open Format in
    match op with
    | OpMove path ->
        fprintf fmt "moving %a" pp_path path
    | OpBorrow(Access.Imm, path) ->
        fprintf fmt "borrowing %a immutably" pp_path path
    | OpBorrow(Access.Mut, path) ->
        fprintf fmt "borrowing %a mutably" pp_path path
    | OpAssign path ->
        fprintf fmt "assignment to %a" pp_path path
    | OpScope name ->
        fprintf fmt "%s going out of scope" name
    | OpReturn func ->
        fprintf fmt "returning from function %s" func
    | OpResultLet ->
        fprintf fmt "returning result of let-expression"

let pp_error fmt exn =
    let open Format in
    match exn with
    | Expr.UnboundVariable var ->
        fprintf fmt "unbound variable %s" var
    | Expr.AccessDeadPath(bad, reason) ->
        fprintf fmt "@[<hv2>failed %a, due to %a@]"
            pp_op bad pp_op reason
    | Expr.PermissionDenied op ->
        fprintf fmt "no enough permission for %a" pp_op op
    | Expr.InvalidPath(path, typ, Access.Deref :: _) ->
        fprintf fmt "cannot deref %a : %a"
            pp_path path pp_typ typ
    | Expr.InvalidPath(path, typ, Access.Field k :: _) ->
        fprintf fmt "no such field \"%d\" in %a : %a"
            k pp_path path pp_typ typ
    | _ ->
        fprintf fmt "fatal error %s" (Printexc.to_string exn)
