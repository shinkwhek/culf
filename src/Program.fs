module Program

open System
module Lexer =
    open Lexer
    let run s = lex s

module Parser =
    open Parser
    let run l = parse l

[<EntryPoint>]
let main argv =
    argv.[0]
    |> string
    |> Lexer.run
    |> Parser.run
    |> printfn "%A"
    0 // return an integer exit code
