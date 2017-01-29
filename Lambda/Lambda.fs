module Lambda

open Types
open Parser
open Eval

open FParsec

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
      result |> reduceAll |> termstr
    | Failure(errorMsg, _, _) -> 
      ":("

[<EntryPoint>]
let main argv =

    test expParser "λf.λx.f (f (f (f x)))"
    test expParser "λn.λf.λx.f (n f x)"
    test expParser "λf.λx.f x y z"
    test expParser "n f x"

    let zeroStr = "λf.λx.x"
    let succStr = "λn.λf.λx.f (n f x)"

    let idStr = "λx.x"
    let idIdStr = sprintf "%s %s" idStr idStr

    //test expParser zeroStr
    //test expParser succStr

    let sz = sprintf "(%s) %s" succStr zeroStr
    let sz' = sprintf "%s %s" succStr zeroStr
    let ssz = sprintf "(%s) ((%s) %s)" succStr succStr zeroStr

    //test expParser sz

    test expParser sz
    test expParser sz'
    test expParser idIdStr
    //testReduce expParser ssz

    0 // return an integer exit code
