open System

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>) 

let run p str =
    let (Parser fn) = p
    fn str

let bindP f p =
    let innerFn str =
        match run p str with
        | Failure e -> Failure e
        | Success (r,s) ->
            match run (f r) s with
            | Failure e -> Failure e
            | Success (r1, s1) -> Success (r1, s1)
    Parser innerFn

let ( >>= ) p f = bindP f p

let returnP x =
    let innerFn str =
        Success (x, str)
    Parser innerFn

let pchar chr =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            Failure "no more input"
        else if str[0] = chr then
            Success (chr, str[1..])
        else
            Failure $"expecting {chr} found {str[0]}"
    Parser innerFn

let andThen p1 p2 =
    p1 >>= (fun r1 -> 
    p2 >>= (fun r2 ->
        returnP (r1, r2)))

let ( .>>. ) = andThen

let orElse p1 p2 =
    let innerFn str =
        let r1 = run p1 str
        match r1 with
        | Success (c, chrs) -> Success (c, chrs)
        | Failure _ -> run p2 str 
    Parser innerFn

let ( <|> ) = orElse

let choice parsers =
    List.reduce (<|>) parsers

let anyOf chrs =
    chrs
    |> List.map pchar
    |> choice

let mapP f p = bindP (f >> returnP) p

let ( <!> ) = mapP
let ( |>> ) x f = mapP f x

let applyP fp xp = 
    fp >>= (fun f ->
    xp >>= fun r ->
    returnP (f r))
    
let ( <*> ) = applyP

let lift2 f p1 p2 = returnP f <*> p1 <*> p2

let rec sequence p =
    let cons head tail = head::tail
    let consP = lift2 cons
    match p with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

let charListToStr chrList = chrList |> List.toArray |> System.String 

let pstring str =
    str 
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToStr

let rec matchN p =
    let innerFn str =
        let result = run (p .>>. (matchN p)) str
        match result with
        | Failure _ -> Success ([], str)
        | Success ((v1, v2), r) -> Success (v1::v2, r)
    Parser innerFn

let many p = matchN p

let many1 p =
    p >>= (fun r1 -> 
    (many p) >>= (fun r2 ->
    returnP (r1::r2)))

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