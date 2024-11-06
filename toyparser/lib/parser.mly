%{
open Ast
%}

%token <string> CONST
%token PLUS
%token LESS
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left LESS

%start <ast> prog
%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e3 = expr; LESS; e4 = expr { Less(e3,e4)}
  | LPAREN; e=expr; RPAREN {e}
;
