#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp

open System
open FParsec

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

let varNameParser = 
  many1 lower |>> (fun cs -> new String(List.toArray(cs)))

let varParser = varNameParser |>> Var
let lamParser = 
        pipe2 ((pchar 'λ' <|> pchar '\\') >>. varNameParser) 
              (pchar '.' >>. expParser)
              (fun s e -> Lam (s, e))
let parParser = between (pchar '(') (pchar ')') expParser
let notAppParser = lamParser <|> varParser <|> parParser

let appParser = 
  let applify defs =
    let leftest = List.head defs
    let restest = List.tail defs
    List.fold (fun acc e -> App (acc, e)) leftest restest 
  sepBy1 notAppParser (pchar ' ') |>> applify

do expParserRef := attempt appParser <|> notAppParser

let unparse =
    let pstr s = "(" + s + ")"
    let rec unparse = function
        | Lam (p, b) -> "λ" + p + "." + unparse b
        | App (Lam (p, b), a) -> pstr (unparse (Lam (p, b))) + " " + argstr a
        | App (f, a) -> unparse f + " " + argstr a
        | Var s -> s
    and argstr = function
        | Var s -> s
        | t -> pstr (unparse t)
    unparse

let rec subst = function
    | t, s, App (f, a) -> App (subst (t, s, f), subst (t, s, a))
    | t, s, Lam (p, b) ->
        if p = s
        then Lam (p, b)
        else Lam (p, subst (t, s, b))
    | t, s, Var v ->
        if v = s
        then t
        else Var v

let rec reduce = function
    | App (Lam (p, b), a) -> subst (a, p, b)
    | App (f, a) -> App (reduce f, reduce a)
    | Lam (p, b) -> Lam (p, reduce b)
    | Var v -> Var v

let rec reduceAll t =
    let res = reduce t
    if res = t
    then res
    else reduceAll res

let test p str =
    printfn "%s -> " str 
    match run p str with
    | Success(result, _, _)   -> printfn "%A" result
    | Failure(errorMsg, _, _) -> printfn "%s" errorMsg

let testReduce p str =
    printfn "%s -> " str 
    match run p str with
    | Success(result, _, _)   -> printfn " %A" (result |> reduceAll)
    | Failure(errorMsg, _, _) -> printfn " %s" errorMsg

let what p str = 
    match run p str with
    | Success(result, _, _)   -> 
      result |> reduceAll |> unparse
    | Failure(errorMsg, _, _) -> 
      ":("

let lambda = what expParser

let rec repl () =
    let s = Console.ReadLine()
    match s with 
    | null -> Console.WriteLine("?")
    | s' -> 
      s' |> what expParser |> Console.WriteLine
    repl ()

let zeroStr = "λf.λx.x"
let succStr = "λn.λf.λx.f (n f x)"

repl()

