namespace Culf

open System
open System.Collections.Generic

module Lexer =
  type Token =
    | Unknown
    | Eof
    | Num of int
    | Plus

  let mutable pos: int = 0

  let getList (s: String) : Char list =
    List.ofSeq s

  let isEof (l: Char list) =
    pos >= l.Length

  let step () =
    pos <- pos + 1;

  let peek (l: Char list) =
    if isEof l
    then Error pos
    else Ok l.[pos]

  let cutToken (l: Char list) (cond: Char -> bool) =
    try
      let mutable lst : Char list = [];
      while (not (isEof l)) && cond(l.[pos]) do
        lst <- List.append lst [l.[pos]];
        step();
      lst |> Array.ofList |> String |> Ok
    with
      | _ -> Error pos     

  let symbol (l: Char list) (v: Token list) : Token list =
    let kind c : Token =
      match c with
      | '+' -> Token.Plus
      | _ -> Token.Unknown

    let token = match peek l with
                | Ok(c) -> kind c
                | Error(e) ->
                  begin
                    eprintfn "cannot peek charactor: pos: %d" e;
                    Token.Unknown
                  end
    step()
    List.append v [token]

  let num (l: Char list) (v: Token list) : Token list =
    let token = match cutToken l System.Char.IsDigit with
                  | Ok s -> s |> int |> Token.Num
                  | Error e ->
                    begin 
                      eprintfn "cannot read num token: pos: %d" e
                      Token.Unknown
                    end
    List.append v [token] 

  let rec token (l: Char list) (v: Token list) : Token list =
    let kind c = match c with
                  | n when System.Char.IsDigit n -> v |> num l |> token l
                  | c -> v |> symbol l |> token l
  
    if not (isEof l)
    then match peek l with
          | Ok c -> kind c
          | Error e ->
            begin
              eprintfn "cannot peek charactor: pos: %d" e
              List.append v [Token.Unknown]
           end
    else v

  let lex (s: String) : Token list =
    let sl = s |> getList
    let v: Token list = []
    v |> token sl