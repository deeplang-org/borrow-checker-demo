
open Checker

let run_src fmt src =
    let expr = Parser.top Lexer.token (Lexing.from_string src) in
    match Expr.check Expr.Env.empty (Access.empty_cset, []) expr with
    | _ ->
        Format.fprintf fmt "this program is well typed"
    | exception exn ->
        Format.fprintf fmt "type checking failed.@ %a"
            Pretty.pp_error exn


let _ =
    let open Js_of_ocaml in
    Js.export "checker"
        (object%js
            method run margin src =
                let fmt = Format.str_formatter in
                Format.pp_set_margin fmt margin;
                Format.fprintf fmt "@[<v>";
                run_src fmt (Js.to_bytestring src);
                Format.fprintf fmt "@]";
                Format.pp_print_flush fmt ();
                let output = Buffer.contents Format.stdbuf in
                Buffer.reset Format.stdbuf;
                Js.bytestring output
        end)
