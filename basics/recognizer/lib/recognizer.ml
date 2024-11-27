type word = char list;;

let charlist_of_string (lst : string) : word =
  lst |> String.to_seq |> List.of_seq;;
  
let lang1 w = Str.string_match (Str.regexp "^[01]+$") w 0;;

let lang2 w = Str.string_match (Str.regexp "^0?1*$") w 0;;

let lang3 w = Str.string_match (Str.regexp "^0[01]*0$") w 0;;

let lang4 w = Str.string_match (Str.regexp "^0*10*10*$") w 0;;

let lang5 w = Str.string_match (Str.regexp "^\\(00\\|11\\)+$") w 0;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5];;
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;
  
