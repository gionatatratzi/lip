type ast =
    Const of int
  | Add of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Div of ast * ast
  | None


