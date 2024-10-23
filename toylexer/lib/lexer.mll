{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }   
  | [ 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' ]*    { ATOK (Lexing.lexeme lexbuf) }
  | [ 'a' | 'e' | 'i' | 'o' | 'u' ]+            { BTOK (Lexing.lexeme lexbuf) }
  | [ 'a'-'z' 'A'-'Z' ]*                        {
      let s = Lexing.lexeme lexbuf in
      let vowel_count = String.fold_left (fun acc c -> if is_vowel c then acc + 1 else acc) 0 s in
      if vowel_count <= 1 then CTOK s else ID s
    }
  | '-'? [ '0'-'9' ]* '.' [ '0'-'9' ]+          { DTOK (Lexing.lexeme lexbuf) }
  | '0' [ 'x' | 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]+ { ETOK (Lexing.lexeme lexbuf) }
  | [ 'a'-'z' 'A'-'Z' '0'-'9' ]+                { ID (Lexing.lexeme lexbuf) }   (* token ID standard *)
  | [ '0'-'9' ]+                                { CONST (int_of_string (Lexing.lexeme lexbuf)) } (* token CONST standard *)
  | eof { EOF }

