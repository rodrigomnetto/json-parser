open System

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>) 

let pchar chr =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            Failure "no more input"
        else if str[0] = chr then
            Success ($"found {chr}", str[1..])
        else
            Failure $"expecting {chr} found {str[0]}"
    Parser innerFn

let run parser str =
    let (Parser fn) = parser
    fn str

let andThen parser1 parser2 =
    let innerFn str =
        let r1 = run parser1 str
        match r1 with
            | Failure msg -> Failure msg
            | Success (c1, chrs) -> 
                let r2 = run parser2 chrs
                match r2 with
                | Failure msg -> Failure msg
                | Success (c2, chrs) -> Success ((c1, c2), chrs)
    Parser innerFn

let ( .>>. ) = andThen

let orElse parser1 parser2 =
    let innerFn str =
        let r1 = run parser1 str
        match r1 with
        | Success (c, chrs) -> Success (c, chrs)
        | Failure _ -> run parser2 str 
    Parser innerFn

let ( <|> ) = orElse

let choice parsers =
    List.reduce (<|>) parsers


let anyOf chrs =
    chrs
    |> List.map pchar
    |> choice





let parseA = pchar 'A'
let parseB = pchar 'B'
let parseC = pchar 'C'
let result = run (parseC .>>. (parseA <|> parseB)) "CABbc" 
printfn $"{result}"
