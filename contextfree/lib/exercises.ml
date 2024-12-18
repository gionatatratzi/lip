open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S1";  (* Regola: aggiungi una coppia 0 e 1 intorno a S *)
        S --> "";     (* Regola: parola vuota *)
      ];
    start = S;        (* Simbolo iniziale: S *)
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";  (* Regola: aggiungi 0 ai bordi *)
        S --> "1S1";  (* Regola: aggiungi 1 ai bordi *)
        S --> "0";    (* Palindromo base: un singolo 0 *)
        S --> "1";    (* Palindromo base: un singolo 1 *)
        S --> "";     (* Parola vuota *)
      ];
    start = S;        (* Simbolo iniziale: S *)
  }


(* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar =
  {
    symbols = [ S ];
    terminals = [ '('; ')'; '['; ']'; '{'; '}' ];
    productions =
      [
        S --> "(S)";  (* Parentesi tonde bilanciate *)
        S --> "[S]";  (* Parentesi quadre bilanciate *)
        S --> "{S}";  (* Parentesi graffe bilanciate *)
        S --> "SS";   (* Concatenazione di due strutture bilanciate *)
        S --> "";     (* Parola vuota *)
      ];
    start = S;        (* Simbolo iniziale: S *)
  }


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S1";  (* Aggiungi una coppia 0 e 1 bilanciata *)
        S --> "1S0";  (* Aggiungi una coppia 1 e 0 bilanciata *)
        S --> "SS";   (* Concatenazione di due strutture bilanciate *)
        S --> "";     (* Parola vuota *)
      ];
    start = S;        (* Simbolo iniziale: S *)
  }
