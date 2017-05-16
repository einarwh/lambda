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
  | Failure (x, _, _) -> failwith x

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

let varNameParser : Parser<String, unit> = 
  many1 lower |>> (fun cs -> new String(List.toArray(cs)))

let varParser = varNameParser |>> Var

let lamParser = 
  let lp1 = (pchar 'λ') >>. varNameParser
  let lp2 = (pchar '.') >>. expParser
  pipe2 lp1 lp2 (fun s e -> Lam (s, e))

do expParserRef := lamParser <|> varParser

// λx.x

