%{
open Expr
%}

%token<int> INT
%token<string> IDENT

%token LPAREN RPAREN
%token AMPERSAND
%token COMMA
%token SEMICOLON
%token DOT
%token STAR
%token EQ COLONEQ

%token LET MUT
%token IF THEN ELSE

%token EOF

%type<Expr.expr> top

%start top

%%
top :
    | expr EOF { $1 }
;

expr :
    | LET IDENT EQ simple_expr SEMICOLON expr
        { Let($2, $4, $6) }
    | simple_expr SEMICOLON expr
        { Let("_", $1, $3) }
    | simple_expr
        { $1 }
;

simple_expr :
    | INT
        { Int $1 }
    | path
        { Path(Type.POwner, $1) }
    | AMPERSAND MUT path
        { Path(Type.PBorrow Mut, $3) }
    | AMPERSAND path
        { Path(Type.PBorrow Imm, $2) }
    | path COLONEQ simple_expr
        { Assign($1, $3) }
    | LPAREN tuple RPAREN
        { match $2 with
          | [expr] -> expr
          | exprs  -> Tuple exprs }
    | IF simple_expr THEN simple_expr ELSE simple_expr
        { If($2, $4, $6) }
;

tuple :
    | expr
        { [$1] }
    | expr COMMA tuple
        { $1 :: $3 }
;

path :
    | path_rev
        { let (var, sels) = $1 in
          (var, List.rev sels) }
;
 
path_rev :
    | simple_path_rev
        { $1 }
    | STAR path_rev
       { let (var, sels) = $2 in
         (var, Access.Deref :: sels) }
;

simple_path_rev :
    | IDENT
       { ($1, []) }
    | simple_path_rev DOT INT
       { let (var, sels) = $1 in
         (var, Access.Field $3 :: sels) }
    | LPAREN path_rev RPAREN
       { $2 }
;
