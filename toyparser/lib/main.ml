open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int

let string_of_result n = string_of_int n
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) -> n
  | Add(e1,e2) -> eval e1 + eval e2
  | Less(e1,e2) -> eval e1 - eval e2
  | Mult(e1,e2) -> eval e1 * eval e2
  | Div(e1,e2)  -> let v2 = eval e2 in eval e1 / eval e2
  | _ -> failwith "undefined"


                    
