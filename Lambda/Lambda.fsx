#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp

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

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

let varNameParser : Parser<String, unit> = 
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

let rec subst = function
    | x, s, App (f, a) -> App (subst (x, s, f), subst (x, s, a))
    | x, s, Lam (p, b) ->
        if p = s
        then Lam (p, b)
        else Lam (p, subst (x, s, b))
    | x, s, Var v ->
        if v = s
        then x
        else Var v

type EvalResult = Next of Exp | Normal

let rec reduce = function
    | Var v -> Normal
    | App (Lam (p, b), a) -> Next (subst (a, p, b))
    | App (f, a) ->
        match reduce f with
            | Next rf -> Next (App (rf, a))
            | _ ->
                match reduce a with
                    | Next ra -> Next (App (f, ra))
                    | _ -> Normal
    | Lam (p, b) ->
        match reduce b with
            | Next b -> Next (Lam (p, b))
            | _ -> Normal

let test p str =
    printfn "%s -> " str 
    match run p str with
    | Success(result, _, _)   -> printfn "%A" result
    | Failure(errorMsg, _, _) -> printfn "%s" errorMsg

let testReduce p str =
    printfn "%s -> " str 
    match run p str with
    | Success(result, _, _)   -> printfn " %A" (result |> reduce)
    | Failure(errorMsg, _, _) -> printfn " %s" errorMsg

let rec runEval x =
    match reduce x with
        | Normal -> Console.WriteLine (unparse x)
        | Next nx ->
            Console.WriteLine (unparse nx)
            runEval nx

let what p str = 
    match run p str with
    | Success(result, _, _)   -> 
      runEval result
    | Failure(errorMsg, _, _) -> 
      Console.WriteLine ":("

let rec repl () : unit =
    let s = Console.ReadLine()
    match s with 
    | null -> Console.WriteLine("?")
    | s' -> 
      s' |> what expParser |> Console.WriteLine
    repl ()

repl ()