module Parser
open Lexer

type binop =
    | Add

type Ast =
    | Unknown
    | Number of int
    | BinOp of binop * Ast * Ast

let mutable pos = 0

let isEof (l: Lexer.Token list) =
    pos >= l.Length

let step () =
    pos <- pos + 1

let peek (l: Lexer.Token list) =
    if isEof l
    then Error pos
    else Ok l.[pos]

let number (l: Lexer.Token list) : Ast =
    let kind t =
        match t with
        | Lexer.Token.Num n -> 
            begin    
                step ()
                Ast.Number n
            end
        | e ->
            begin
                eprintfn "token %O is not Num" e
                Ast.Unknown
            end
    
    match peek l with
    | Ok t -> kind t
    | Error n ->
        begin
            eprintfn "cannot peek token: %d" n
            Unknown
        end

let rec expr_op1 (l: Lexer.Token list) : Ast =
    let kind t n =
        match t with
        | Lexer.Token.Plus ->
            begin
                step ()
                BinOp(binop.Add, n, (expr_op1 l))
            end
        | _ -> n

    let n = number l
    if not (isEof l) then
        match peek l with
            | Ok t -> kind t n
            | Error e ->
                begin
                    eprintfn "cannot peek token: %d" e
                    Unknown
                end
    else n

let expr (l: Lexer.Token list) : Ast = expr_op1 l

let parse (l: Lexer.Token list) : Ast =
    pos <- 0
    expr l