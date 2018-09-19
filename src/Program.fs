module Program

open System
module Lexer =
    open Lexer
    let run s = lex s

module Parser =
    open Parser
    let run l = parse l

module Eval =
    open Eval
    let run a = eval a

[<EntryPoint>]
let main argv =
    argv.[0]
    |> string
    |> Lexer.run
    |> Parser.run
    |> Eval.run
    |> printfn "%A"
    0 // return an integer exit code
