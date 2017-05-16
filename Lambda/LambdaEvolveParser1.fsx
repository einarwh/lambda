#I @"../packages/FParsec/lib/net40-client"

#r @"FParsec.dll"
#r @"FParsecCS.dll"

open System
open FParsec

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp

let get p s = 
  match run p s with 
  | Success (e, _, _) -> e
  | Failure (x, y, z) -> 
    failwith x

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

let varNameParser : Parser<String, unit> = 
  many1 lower |>> (fun cs -> new String(List.toArray(cs)))

let varParser = 
  printfn "varParser"
  varNameParser |>> Var

let lamParser = 
  printfn "lamParser"
  let lp1 = (pchar 'λ') >>. varNameParser
  let lp2 = (pchar '.') >>. expParser
  pipe2 lp1 lp2 (fun s e -> Lam (s, e))

let parParser = 
  between (pchar '(') (pchar ')') expParser

let notAppParser = lamParser <|> varParser <|> parParser

let appParser = 
  printfn "appParser"
  let applify (leftest::restest) =  
    List.fold (fun acc e -> App (acc, e)) leftest restest
  sepBy1 notAppParser (pchar ' ') |>> applify

expParserRef := attempt appParser <|> notAppParser

// λx.x


