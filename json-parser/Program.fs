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
            Success (chr, str[1..])
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


let mapP f parser =
    let innerFn input =
        let result = run parser input
        match result with
        | Success (v, remaining) -> Success (f v, remaining)
        | Failure err -> Failure err
    Parser innerFn


let ( <!> ) = mapP
let ( |>> ) x f = mapP f x

let returnP x =
    let innerFn str =
        Success (x, str)
    Parser innerFn

let applyP fP xP =
    (fP .>>. xP) |>> fun (f, x) -> f x
    
let ( <*> ) = applyP

let lift2 f p1 p2 = returnP f <*> p1 <*> p2

let rec sequence parsers =
    let cons head tail = head::tail
    let consP = lift2 cons
    match parsers with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

let charListToStr chrList = chrList |> List.toArray |> System.String 

let pstring str =
    str 
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToStr

let rec matchN parser =
    let innerFn str =
        let result = run (parser .>>. (matchN parser)) str
        match result with
        | Failure _ -> Success ([], str)
        | Success ((v1, v2), r) -> Success (v1::v2, r)
    Parser innerFn

let many parser = matchN parser
let many1 parser = 
    let innerFn str =
        match run (parser .>>. matchN parser) str with
            | Failure err -> Failure err
            | Success ((v1, v2), r) -> Success (v1::v2, r)
    Parser innerFn

//optional parser
let opt p =
  let some = p |>> Some
  let none = returnP None
  some <|> none

//int parser
let pint =
    let strToInt (signal, str) =
        let value = str |> List.toArray |> System.String |> int
        match signal with
        | Some _ -> -value
        | _ -> value

    let digitsP = anyOf ['0'..'9']
    let manyDigitsP = many1 digitsP

    ((opt (pchar '-')) .>>. manyDigitsP) |>> fun r -> strToInt r


let (>>.) p1 p2 =
    (p1 .>>. p2)
    |> mapP (fun (a, _) -> a) 

let (.>>) p1 p2 =
    (p1 .>>. p2)
    |> mapP (fun (_, b) -> b) 

let between p1 p2 p3 =
  p1 >>. p2 .>> p3


let sepBy p sp =
    many (p >>. opt sp)

let sepBy1 p sp =
    many1 (p >>. opt sp)

//implementar bind

let comma = pchar ','
let digit = anyOf ['0'..'9']

let oneOrMoreDigitList = sepBy1 pint comma

let result = run oneOrMoreDigitList "145,234,322;"      // Success (['1'], ";")
printfn $"{result}"

//"1,2,3;"

//test

(*
let concat (chr1 : char) (chr2 : char)  = $"{chr1}{chr2}"

let lift2 = (returnP concat) <*> pchar 'A' <*> pchar 'B'
let result = run lift2 "Abcd" 

printfn $"{result}"

let parseDigit = anyOf ['0'..'9']
let tupleToStr ((c1, c2), c3) = System.String [| c1; c2; c3 |] 
let parseThreeDigitsAsStr = (parseDigit .>>. parseDigit .>>. parseDigit) |>> tupleToStr

let parseThreeDigitsAsInt =
  mapP int parseThreeDigitsAsStr

let a = run parseThreeDigitsAsInt "123abc"
    
printf $"{a}"

let parseA = pchar 'A'
let parseB = pchar 'B'
let parseC = pchar 'C'
let result = run (parseC .>>. (parseA <|> parseB)) "CABbc" 
printfn $"{result}"
*)