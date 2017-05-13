module Lambda

open Types
open Parser
open Unparser
open Eval

open FParsec

open System

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
        | Reduced rx ->
            Console.WriteLine (unparse rx)
            runEval rx

let what p str = 
    match run p str with
    | Success(result, _, _)   -> 
      runEval result
    | Failure(errorMsg, _, _) -> 
      Console.WriteLine ":("

let rec repl () =
    let s = Console.ReadLine()
    match s with 
    | null -> Console.WriteLine("?")
    | s' -> 
      s' |> what expParser |> Console.WriteLine
    repl ()

[<EntryPoint>]
let main argv =
    Console.InputEncoding <- System.Text.Encoding.UTF8;
    Console.OutputEncoding <- System.Text.Encoding.UTF8;
    test expParser "λf.λx.f (f (f (f x)))"
    test expParser "λn.λf.λx.f (n f x)"
    test expParser "λf.λx.f x y z"
    test expParser "n f x"

    let zeroStr = "λf.λx.x"
    let succStr = "λn.λf.λx.f (n f x)"

    // zero (\f.\x.x)
    // 2 (\f.\x.f (f x))
    // 3 (\f.\x.f (f (f x)))

    // succ \n.\f.\x.f (n f x)
    // add \a.\b.a (\n.\f.\x.f (n f x)) b

    let idStr = "λx.x"
    let idIdStr = sprintf "%s %s" idStr idStr

    let sz = sprintf "(%s) %s" succStr zeroStr
    let sz' = sprintf "%s %s" succStr zeroStr
    let ssz = sprintf "(%s) ((%s) %s)" succStr succStr zeroStr

    repl ()

    0 // return an integer exit code
