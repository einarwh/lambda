#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec

(* 
   type for expressions
*)

(* 
    how to parse some string into some Exp?

    hello fparsec, useful parser combinator library
*)

(*  helper function for applying some parser to a string *)

(*  a recursive parser for expressions *)

(*
    simple cases: not applications
*)

(*
    Var of string 
*)

parse expParser "foo" = Var "foo"

(*
    Lam of string * Exp
*)

parse expParser "λx.x" = Lam ("x", Var "x")
parse expParser "λa.λb.a" = Lam ("a", Lam ("b", Var "a"))

(*
    Parentheses for grouping
*)

parse expParser "(λx.x)" = Lam ("x", Var "x")

(*
    slightly ickier but not very icky
    App of Exp * Exp
*)

(*
    beware infinite regress!
    parse Exp -> parse App (Exp, Exp) -> parse Exp -> parse App (Exp, Exp)...
*)

parse expParser "x x" = App (Var "x", Var "x")
parse expParser "a b c" = App (Var "a", Var "b"), Var "c"

let appExpString = "(λa.λb.a b a) (λx.λy.x) foo"

let appExp = App (App (Lam ("a", Lam ("b", (App (App (Var "a", Var "b"), Var "a")))), Lam ("x", Lam ("y", Var "x"))), Var "foo")

parse expParser appExpString = appExp


(*
    paren (s: string) : string
    (string -> string)
    put parentheses round s
*)

let paren s = "(" + s + ")"

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

let rec unparse = function
    | Lam (p, x) -> "λ" + p + "." + unparse x
    | Var s -> s
    | App (Lam (p, x), a) -> paren (unparse (Lam (p, x))) + " " + argstring a
    | App (f, a) -> unparse f + " " + argstring a
and argstring = function
    | Var s -> s
    | x -> paren (unparse x)


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

let rec subst = function
    | arg, s, Var v ->
        if v = s then arg else Var v
    | arg, s, Lam (p, x) ->
        if p = s
        then Lam (p, x)
        else Lam (p, subst (arg, s, x))
    | arg, s, App (f, a) ->
        App (subst (arg, s, f), subst (arg, s, a))


appExpString

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

type EvalResult = Normal | Next of Exp

let rec reduce = function
    | Var v -> Normal
    | App (Lam (p, x), a) ->
        Next (subst (a, p, x))
    | App (f, a) ->
        match reduce f with
            | Next nf -> Next (App (nf, a))
            | Normal ->
                match reduce a with
                    | Next na -> Next (App (f, na))
                    | Normal -> Normal
    | Lam (p, x) ->
         match reduce x with
            | Next nx -> Next (Lam (p, nx))
            | Normal -> Normal

match reduce appExp with Next x -> unparse x

(*
    runEval (x: Exp) : unit
    (Exp -> unit)
    call reduce until it returns Normal
    print intermediate results (the Next bits)
*)

let rec runEval x =
    match reduce x with
        | Next nx ->
            Console.WriteLine (unparse nx)
            runEval nx
        | Normal -> ()

runEval (appExp)
runEval (lamExp1)


