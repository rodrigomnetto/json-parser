open System

type ParserLabel = string
type ParserError = string

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError

type Parser<'a> = {
    parseFn: string -> ParseResult<'a * string>
    label: ParserLabel
}

let run p str = p.parseFn str

let setLabel p newLabel =
    let innerFn str =
        match run p str with
        | Success r -> Success r
        | Failure (_, message) ->
        Failure (newLabel, message)
    { parseFn=innerFn; label=newLabel }

let (<?>) p newLabel = setLabel p newLabel

let printResult r = 
    match r with
    | Success (v, _) -> printfn "%A" v
    | Failure (label, message) -> printfn $"Error parsing {label}, {message}"

let bindP f p =
    let innerFn str =
        match run p str with
        | Failure (l, e) -> Failure (l, e)
        | Success (r,s) ->
            match run (f r) s with
            | Failure (l, e) -> Failure (l, e)
            | Success (r1, s1) -> Success (r1, s1)
    { parseFn=innerFn; label="unknown" }

let ( >>= ) p f = bindP f p

let returnP x =
    let label = sprintf "%A" x
    let innerFn str =
        Success (x, str)
    {parseFn=innerFn; label=label}

let satisfy predicate label =
    let innerFn input =
        if String.IsNullOrEmpty(input) then
            Failure (label, "no more input")
        else if predicate input[0] then
            Success (input[0], input[1..])
        else Failure (label, $"Unexpected {input[0]}")
    { parseFn=innerFn; label=label }

let pchar chr =
    let predicate = (fun x -> chr = x)
    satisfy predicate $"{chr}"

let andThen p1 p2 =
    let label = sprintf "%s andThen %s" p1.label p2.label
    p1 >>= (fun r1 -> 
    p2 >>= (fun r2 ->
        returnP (r1, r2))) <?> label

let ( .>>. ) = andThen

let orElse p1 p2 =
    let label = sprintf "%s orElse %s" p1.label p2.label
    let innerFn str =
        let r1 = run p1 str
        match r1 with
        | Success (c, chrs) -> Success (c, chrs)
        | Failure _ -> run p2 str 
    { parseFn=innerFn; label=label }

let ( <|> ) = orElse

let choice parsers =
    List.reduce (<|>) parsers

//colador
let anyOf chrs =
    let label = $"any of {chrs}"
    chrs
    |> List.map pchar
    |> choice
    <?> label

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
    let label = $"many {p.label}"
    let innerFn str =
        let result = run (p .>>. (matchN p)) str
        match result with
        | Failure _ -> Success ([], str)
        | Success ((v1, v2), r) -> Success (v1::v2, r)
    { parseFn=innerFn; label=label }

let many p = matchN p

let many1 p =
    let label = $"many1 {p.label}"
    p >>= (fun r1 -> 
    (many p) >>= (fun r2 ->
    returnP (r1::r2))) 
    <?> label

//optional parser
let opt p =
  let some = (p |>> Some) <?> $"{p.label}"
  let none = returnP None
  some <|> none

//int parser
let pint =
    let strToInt (signal, str) =
        let value = str |> List.toArray |> System.String |> int
        match signal with
        | Some _ -> -value
        | _ -> value

    let digitsP = satisfy Char.IsDigit $"anyOf {['0'..'9']}"
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