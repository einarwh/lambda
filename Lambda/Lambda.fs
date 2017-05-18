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
        | Next nx ->
            Console.WriteLine (unparse nx)
            runEval nx

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
    repl ()
    0 
