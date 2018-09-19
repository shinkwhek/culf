module Eval
open System
open Parser

let num ast =
    match ast with
    | Parser.Number n -> n
    | e ->
        begin
            eprintfn "expect num but : %A" e
            exit 1
        end 
 
let rec eval ast =
    let binop ast =
        let kind t l r =
            match t with
            | Parser.binop.Add ->
                begin
                    (eval l) + (eval r)
                end

        match ast with
        | Parser.BinOp(t, l, r) -> kind t l r
        | _ -> num ast
    
    binop ast