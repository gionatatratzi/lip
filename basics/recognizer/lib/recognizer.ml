type word = char list;;

let charlist_of_string (lst : string) : word =
  lst |> String.to_seq |> List.of_seq;;

(*lang col pattern matching*) 
(*  1. `[01]+`
    2. `0?1*`
    3. `0[01]*0`
    4. `0*10*10*`
    5. `(00|11)+ 
*)

(*  1. `[01]+` *)
let rec lang1 = function
    [] -> false
  | hd::tl -> hd = 0 || hd = 1 || lang1 tl
;;

(*  2. `0?1*` *)
let rec lang2 = function
    [] -> true
  | 0::l -> not(List.mem 0 l) && lang2 l
  | 1::l -> not(List.mem 0 l) && lang2 l
  | _ -> false
;;  

(* 3. `0[01]*0` *)
let rec lang3 = function
    [] | [1] -> false
  | 0::l -> (l = [0] || l = [1]) && lang3 l
  | hd::tl -> if tl = [] then hd = 0 else false
;;

(* 4. `0*10*10*` *)
let rec lang4 = function
    [] -> false
  | 0::tl
  | 1::tl ->  if tl <> [] then (tl = [1] || tl = [0]) && lang4 tl else false
  | hd::tl -> if tl = [] then (hd = 1 || hd = 0) else false
;;

(* 5. `(00|11)+ *)
let rec lang5 = function
    [] -> false
  | 0::0::l -> true && lang5 l
  | 1::1::l -> true && lang5 l
  | _ -> false
;;

let recognizers = [lang1;lang2;lang3;lang4;lang5];;
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;
  
