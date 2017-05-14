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

