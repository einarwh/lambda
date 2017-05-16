#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec


let appExpString = "(λa.λb.a b a) (λx.λy.x) foo"

let lamExp1 = Lam ("a", Lam ("b", (App (App (Var "a", Var "b"), Var "a"))))
let lamExp2 = Lam ("x", Lam ("y", Var "x"))
let appExp = App (App (lamExp1, lamExp2), Var "foo")


(*
    paren (s: string) : string
    (string -> string)
    put parentheses round s
*)


paren "-:   :-"


(*
    unparse (x : Exp) : string
    (Exp -> string)
    turn x into a string

    try to get parentheses in right places:
    if the function in a function application is a lambda

    if the argument in a function application is
        a lambda or a function application
*)


unparse lamExp1
unparse lamExp2
unparse appExp

unparse appExp = appExpString


(*
    subst (arg : Exp, s : String, x : Exp) : Exp
    (Exp * string * Exp -> Exp)
    look for *free* occurences of variable s within x
        and replace them with arg
*)


unparse (subst (Var "horse", "foo", appExp))
unparse (subst (Var "horse", "x", appExp))

(* for testing *)
let get p s = 
  match run p s with 
  | Success (e, _, _) -> e
  | Failure (x, y, z) -> 
    failwith x

(* 
    how to parse some string into some Exp?

    hello fparsec, useful parser combinator library
*)

(*
    1. string -> Var of string 
    get varParser "foo" = Var "foo"
*)

(*
    2. string -> Lam of string * Exp
    get lamParser "λx.x" = Lam ("x", Var "x")
    get lamParser "λa.λb.a" = Lam ("a", Lam ("b", Var "a"))
*)

(*
    3. string -> App of Exp * Exp
    get appParser "x x" = App (Var "x", Var "x")
    get appParser "a b c" = App (Var "a", Var "b"), Var "c"
*)

(*
    beware infinite regress!
    parse Exp -> parse App (Exp, Exp) -> parse Exp -> parse App (Exp, Exp)...
*)

(*
    controlling presedence
    get appParser "a (b c)" = App (Var "a", App (Var "b", Var "c")) ?
*)

(* 
    litmus 
    get expParser appExpString = 
    App
      (App
         (Lam ("a",Lam ("b",App (App (Var "a",Var "b"),Var "a"))),
          Lam ("x",Lam ("y",Var "x"))),Var "foo")

    simple test:
    appExpString = (appExpString |> get expParser |> unparse)
*)

(*
    reduce (x : Exp) : EvalResult
    (Exp -> EvalResult)
    look for a function applications where the function is a lambda

    if we find one: return a Next
        with the body of the lambda
            with function argument substituted for the lambda-parameter

    if we don't find one: return Normal
*)




(*
    runEval (x: Exp) : unit
    (Exp -> unit)
    call reduce until it returns Normal
    print intermediate results (the Next bits)
*)


runEval (appExp)
runEval (lamExp1)

