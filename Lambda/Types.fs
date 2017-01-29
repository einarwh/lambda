module Types

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp