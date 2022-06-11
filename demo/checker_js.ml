
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
            method exampleNames =
                Examples.examples
                |> Array.map (fun (name, _, _) -> Js.string name)
                |> Js.array

            method getExample name =
                let name = Js.to_string name in
                let rec find i =
                    if i >= Array.length Examples.examples
                    then Js.null
                    else
                        let (name', src, _) = Examples.examples.(i) in
                        if name' = name
                        then Js.some (Js.string src)
                        else find (i + 1)
                in
                find 0

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
