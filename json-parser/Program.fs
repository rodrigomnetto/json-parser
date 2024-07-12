open System

type ParserLabel = string
type ParserError = string

type ParsingState = {
    lines: string[]
    line: int
    column: int
}

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParsingState

let incLine state = { state with line = state.line + 1; column = 0 }
let incColumn state = { state with column = state.column + 1 }

let initialize str = 
    if String.IsNullOrEmpty(str) then
        { lines=[||]; line= -1; column= -1 }
    else
        let separators = [| "\r\n"; "\n" |]
        let lines = str.Split(separators, StringSplitOptions.None)
        { lines=lines; line= -1; column= -1 }

let nextChar state = 
    let { lines=lines; line=line; column =column } = state

    if line > -1 && column > -1 then
        let newState = incColumn state

        if newState.column < lines[line].Length then
            (Some lines.[newState.line].[newState.column], newState)
        elif newState.column = lines[line].Length then
            (Some '\n', newState)
        else
            let newState = incLine state
            if newState.line < state.lines.Length then
                (Some lines.[newState.line].[newState.column], newState)
            else
                (None, state)
    else
        if state.lines.Length = 0 then 
            (None, state)
        else
            let newState = incLine state
            (Some lines.[newState.line].[newState.column], newState)


type Parser<'a> = {
    parseFn: ParsingState -> ParseResult<'a * ParsingState>
    label: ParserLabel
}

let runOnParsingState p state = 
    p.parseFn state

let run p str = 
    let state = initialize str
    runOnParsingState p state

let setLabel p newLabel =
    let innerFn state =
        match runOnParsingState p state with
        | Success r -> Success r
        | Failure (_, message, state) ->
        Failure (newLabel, message, state)
    { parseFn=innerFn; label=newLabel }

let (<?>) p newLabel = setLabel p newLabel

let printResult r = 
    match r with
    | Success (v, _) -> printfn "%A" v
    | Failure (label, message, state) -> 
        let { lines=lines; line=line; column =column } = state
        printfn "Error parsing %s at line %d column %d\n%s\n%*s^%s" label (line + 1) (column + 1) (lines.[line]) column "" message

let bindP f p =
    let innerFn state =
        match runOnParsingState p state with
        | Failure (l, e, s) -> Failure (l, e, s)
        | Success (r,s) ->
            match runOnParsingState (f r) s with
            | Failure (l, e, s) -> Failure (l, e, s)
            | Success (r1, s1) -> Success (r1, s1)
    { parseFn=innerFn; label="unknown" }

let ( >>= ) p f = bindP f p

let returnP x =
    let label = sprintf "%A" x
    let innerFn state =
        Success (x, state)
    {parseFn=innerFn; label=label}

let satisfy predicate label =
    let innerFn state =
        match nextChar state with
        | (None, s) -> Failure (label, "no more input", s)
        | (Some c, s) -> 
            if predicate c then
                Success (c, s)
            else 
                Failure (label, $"Unexpected '{c}'", s)
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
    let innerFn state =
        let r1 = runOnParsingState p1 state
        match r1 with
        | Success (c, chrs) -> Success (c, chrs)
        | Failure _ -> runOnParsingState p2 state 
    { parseFn=innerFn; label=label }

let ( <|> ) = orElse

let choice parsers =
    List.reduce (<|>) parsers

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
    let innerFn state =
        let result = runOnParsingState (p .>>. (matchN p)) state
        match result with
        | Failure _ -> Success ([], state)
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

let digitChar = 
    let predicate = Char.IsDigit
    satisfy predicate "digit"

//int parser
let pint =
    let strToInt (signal, str) =
        let value = str |> List.toArray |> System.String |> int
        match signal with
        | Some _ -> -value
        | _ -> value

    let manyDigits = many1 digitChar
    (((opt (pchar '-')) .>>. manyDigits) |>> fun r -> strToInt r) <?> "integer"

let pfloat =
   let chrsToFloat (((signal, integer), point), dec) =
        let value = (List.append integer (point :: dec)) |> List.toArray |> System.String |> float
        match signal with
        | Some _ -> -value
        | _ -> value

   let manyDigits = many1 digitChar
   ((opt (pchar '-') .>>. manyDigits .>>. (pchar '.') .>>. manyDigits) |>> chrsToFloat) <?> "float"

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

let manyChars cp =
  many cp
  |>> charListToStr

let manyChars1 cp =
  many1 cp
  |>> charListToStr

let whiteSpaceChar =
    let predicate = Char.IsWhiteSpace
    satisfy predicate "whitespace"

let spaces = many whiteSpaceChar

let spaces1 = many1 whiteSpaceChar