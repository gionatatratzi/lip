%{
open Ast
%}

%token <string> CONST
%token PLUS
%token LESS
%token MULT
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left LESS
%left MULT
%left DIV

%start <ast> prog
%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; LESS; e2 = expr { Less(e1,e2)}
  | e1 = expr; MULT; e2 = expr { Mult(e1,e2)}
  | e1 = expr; DIV; e2 = expr { Div(e1,e2)}
  | LPAREN; e=expr; RPAREN {e}
;
