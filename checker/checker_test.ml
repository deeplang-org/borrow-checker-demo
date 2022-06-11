
open Checker

let expr_of_lexbuf  lexbuf = Parser.top Lexer.token lexbuf
let expr_of_string  src    = expr_of_lexbuf (Lexing.from_string src)
let expr_of_channel ch     = expr_of_lexbuf (Lexing.from_channel ch)

let run_test (label, src, expected) =
    let open Format in
    let expr = expr_of_string src in
    match Expr.check Expr.Env.empty (Access.empty_cset, []) expr with
    | _ ->
        begin match expected with
        | None ->
            printf "test %s: passed@ " label;
            true
        | Some exn ->
            printf "@[<v2>test %s: failed@ " label;
            printf "expected result: %a@ " Pretty.pp_error exn;
            printf "actual result: well-typed@]@ ";
            false
        end
    | exception exn ->
        match expected with
        | None ->
            printf "@[<v2>test %s: failed@ " label;
            printf "expected result: well-typed@ ";
            printf "actual result: %a@]@ " Pretty.pp_error exn;
            false
        | Some exn' when exn' = exn ->
            printf "test %s: passed@ " label;
            true
        | Some exn' ->
            printf "@[<v2> test %s: failed@ " label;
            printf "expected result: %a@ " Pretty.pp_error exn';
            printf "actual result: %a@]@ " Pretty.pp_error exn;
            false


let _ =
    Format.printf "@[<v>";
    let fail_count = ref 0 in
    Examples.examples |> Array.iter begin fun test ->
        if not (run_test test) then
            incr fail_count
    end;
    Format.printf "summary: %d of %d test passed@]"
        (Array.length Examples.examples - !fail_count)
        (Array.length Examples.examples);
    if !fail_count > 0 then
        exit 1
