#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec


let appExpString = "(λa.λb.a b a) (λx.λy.x) foo"

type Exp = Var of string | Lam of string * Exp | App of Exp * Exp

let lamExp1 = Lam ("a", Lam ("b", (App (App (Var "a", Var "b"), Var "a"))))
let lamExp2 = Lam ("x", Lam ("y", Var "x"))
let appExp = App (App (lamExp1, lamExp2), Var "foo")


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



(* 
    how to parse some string into some Exp?

    hello fparsec, useful parser combinator library
*)

let parse p s = 
  match run p s with 
  | Success (e, _, _) -> e
  | Failure (x, y, z) -> 
    failwith x

(*
    1. string -> Var of string 
    parse varParser "foo" = Var "foo"
*)

let varNameParser : Parser<string, unit> =
  many1 lower |>> (fun cs -> String(List.toArray(cs)))

let varParser =
  varNameParser |>> Var

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

(*
    2. string -> Lam of string * Exp
    parse lamParser "λx.x" = Lam ("x", Var "x")
    parse lamParser "λa.λb.a" = Lam ("a", Lam ("b", Var "a"))
*)

let lamParser = 
  let lp1 = (pchar 'λ') >>. varNameParser
  let lp2 = (pchar '.') >>. expParser
  pipe2 lp1 lp2 (fun n e -> Lam (n, e))

let parParser = 
  between (pchar '(') (pchar ')') expParser

let notAppParser = lamParser <|> parParser <|> varParser

let appParser = 
  chainl1 notAppParser (pchar ' ' |>> (fun _ f a -> App (f, a)))

expParserRef := appParser

(*
    3. string -> App of Exp * Exp
    parse appParser "x x" = App (Var "x", Var "x")
    parse appParser "a b c" = App (Var "a", Var "b"), Var "c"
*)

(*
    beware infinite regress!
    parse Exp -> parse App (Exp, Exp) -> parse Exp -> parse App (Exp, Exp)...
*)

(*
    controlling presedence
    parse appParser "a (b c)" = App (Var "a", App (Var "b", Var "c")) ?
*)

(* 
    litmus 
    parse expParser appExpString = 
    App
      (App
         (Lam ("a",Lam ("b",App (App (Var "a",Var "b"),Var "a"))),
          Lam ("x",Lam ("y",Var "x"))),Var "foo")

    simple test:
    appExpString = (appExpString |> get expParser |> unparse)
*)


