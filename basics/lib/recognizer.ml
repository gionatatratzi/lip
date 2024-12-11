type word = char list

let charlist_of_string (lst : string) : word =
  lst |> String.to_seq |> List.of_seq

(* 1. `[01]+` *)
let rec lang1 = function
  | [] -> false
  | '0'::tl -> lang1 tl || tl = []
  | '1'::tl -> lang1 tl || tl = []
  | _ -> false

(* 2. `0?1*` *)
let rec lang2 = function
  | [] -> true
  | '0'::tl -> List.for_all ((=) '1') tl
  | '1'::tl -> lang2 tl
  | _ -> false

(* 3. `0[01]*0` *)
let lang3 = function
  | [] | ['1'] -> false
  | '0'::tl -> List.exists (fun x -> x = '0') tl
  | _ -> false

(* 4. `0*10*10*` *)
let lang4 = function
  | [] -> false
  | '1'::'0'::'1'::tl -> List.for_all (fun x -> x = '0') tl
  | _ -> false

(* 5. `(00|11)+` *)
let rec lang5 = function
  | [] -> false
  | '0'::'0'::tl -> lang5 tl || tl = []
  | '1'::'1'::tl -> lang5 tl || tl = []
  | _ -> false

let recognizers = [lang1; lang2; lang3; lang4; lang5]

let belongsTo (w: word) = List.map (fun f -> f w) recognizers
