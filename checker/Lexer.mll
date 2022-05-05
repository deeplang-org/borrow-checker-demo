{
    open Parser

    let keywords = Hashtbl.of_seq @@ List.to_seq
    [ "let", LET; "mut" , MUT
    ; "if" , IF ; "then", THEN; "else", ELSE ]
}

let dex_digits = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident_char = '_' | dex_digits | lowercase | uppercase
let blank = [' ' '\t' '\n' '\r']

rule token = parse
    | eof    { EOF }
    | blank+ { token lexbuf }
    | dex_digits+ { INT(int_of_string @@ Lexing.lexeme lexbuf) }
    | ('_'*)(lowercase)(ident_char*)
        { let id = Lexing.lexeme lexbuf in
          match Hashtbl.find keywords id with
          | kw                  -> kw
          | exception Not_found -> IDENT id }
    | '_' { IDENT "_" }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '&' { AMPERSAND }
    | ',' { COMMA }
    | ';' { SEMICOLON }
    | '.' { DOT }
    | '*' { STAR }
    | '=' { EQ }
    | ":=" { COLONEQ }
