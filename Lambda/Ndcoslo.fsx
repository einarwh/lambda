#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec

(* 
   type for expressions
*)

type Exp = Var of string | App of Exp * Exp | Lam of string * Exp

(* 
    how to parse some string into some Exp?

    hello fparsec, useful parser combinator library
*)

(*  helper function for applying some parser to a string *)

let parse p s = 
  match run p s with 
  | Success (e, _, _) -> e
  | Failure (m, _, _) -> failwith m

(*  a recursive parser for expressions *)

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

(*
    simple cases: not applications
*)

(*
    Var of string 
*)

let varNameParser : Parser<string, unit> = 
  many1 (lower <|> upper) |>> (List.toArray >> String)

let varParser = 
  varNameParser |>> Var

expParserRef := varParser

parse expParser "foo" = Var "foo"

(*
    Lam of string * Exp
*)

let lamParser =
  let p1 = (pchar 'λ') >>. varNameParser
  let p2 = (pchar '.') >>. expParser
  pipe2 p1 p2 (fun name body -> Lam (name, body))

expParserRef := lamParser <|> varParser

parse expParser "λx.x" = Lam ("x", Var "x")
parse expParser "λa.λb.a" = Lam ("a", Lam ("b", Var "a"))

(*
    Parentheses for grouping
*)

let parParser =
  between (pchar '(') (pchar ')') expParser

let notAppParser =
  parParser <|> lamParser <|> varParser

expParserRef := notAppParser

parse expParser "(((λx.x)))" = Lam ("x", Var "x")

(*
    slightly ickier but not very icky
    App of Exp * Exp
*)

(*
    beware infinite regress!
    parse Exp -> parse App (Exp, Exp) -> parse Exp -> parse App (Exp, Exp)...
*)

let appParser =
  chainl1 notAppParser (pchar ' ' |>> (fun _ f a -> App (f, a)))

expParserRef := appParser

parse expParser "x x" = App (Var "x", Var "x")
parse expParser "a b c" = App (App (Var "a", Var "b"), Var "c")

parse expParser "a (b c)"

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
    | Var v -> v
    | Lam (p, x) -> "λ" + p + "." + unparse x
    | App (Lam (p, x), a) -> paren (unparse (Lam (p, x))) + " " + argstring a
    | App (f, a) -> unparse f + " " + argstring a
and argstring = function
    | Var v -> v
    | x -> paren (unparse x)

let rec subst = function
| arg, s, Var v -> if v = s then arg else Var v
| arg, s, App (f, a) -> App (subst (arg, s, f), subst (arg, s, a))
| arg, s, Lam (p, x) -> if p = s then Lam (p, x) else Lam (p, subst (arg, s, x)) 

type Result = Normal | Next of Exp

let rec reduce = function
| App (Lam (p, x), a) -> Next (subst (a, p, x))
| Var v -> Normal
| Lam (p, x) ->
    match reduce x with
    | Normal -> Normal
    | Next nx -> Next (Lam (p, nx))
| App (f, a) ->
    match reduce f with
    | Next nf -> Next (App (nf, a))
    | Normal ->
        match reduce a with
        | Next na -> Next (App (f, na))
        | Normal -> Normal

let rec runEval x =
    match reduce x with
    | Normal -> ()
    | Next nx ->
        System.Console.WriteLine (unparse nx)
        runEval nx

let rec repl _ : unit =
    runEval (parse expParser (System.Console.ReadLine ()))
    repl ()

(*
(λx.λy.y) foo bar

0
λf.λx.x

2
λf.λx.f (f x)

3
λf.λx.f (f (f x))

λa.λb.λf.λx.a f (b f x)

(λa.λb.λf.λx.a f (b f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))

*)

