namespace ParserCombinatorLib2

module Main =
    type Result<'T> =
        | Success of 'T * int
        | Error of int

    type Parser<'T> = Parser of (string -> int -> Result<'T>)

    let Run (parser: Parser<'T>) (input: string) (index: int) =
        let (Parser innerFn) = parser
        innerFn input index

    let Parse (parser: Parser<'T>) (input: string) =
        match Run parser input 0 with
        | Success (value: 'T, index) ->
            Some value
        | Error (index) -> 
            printf "Failed parsing at char %d\n" index
            None

    let Satisfies (fn: (char -> bool)) = 
        let innerFn (input: string) (index: int) =
            if (index >= input.Length) then
                Error (index)
            else 
                let c = input[index]
                if (fn(c)) then
                    Success (c, index + 1)
                else
                    Error (index)
        Parser innerFn

    let CharParser (c: char) = Satisfies (fun x -> c = x)

    let Both parser1 parser2 =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser1 input index

            match result1 with
            | Error index -> Error (index)

            | Success (value1, index) ->
                let result2 = Run parser2 input index
                match result2 with
                | Error index -> Error (index)

                | Success (value2, index) ->
                    Success ((value1, value2), index)
        Parser innerFn
    let (.>>.) = Both

    let Left parser1 parser2 =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser1 input index

            match result1 with
            | Error index -> Error (index)

            | Success (value1, index) ->
                let result2 = Run parser2 input index
                match result2 with
                | Error index -> Error (index)

                | Success (_, index) ->
                    Success (value1, index)
        Parser innerFn
    let (.>>) = Left

    let Right parser1 parser2 =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser1 input index

            match result1 with
            | Error index -> Error (index)

            | Success (_, index) ->
                let result2 = Run parser2 input index
                match result2 with
                | Error index -> Error (index)

                | Success (value2, index) ->
                    Success (value2, index)
        Parser innerFn
    let (>>.) = Right

    let Between parser1 parser2 parser3 = parser1 >>. parser2 .>> parser3

    let Either parser1 parser2 =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser1 input index

            match result1 with
            | Error index -> 
                let result2 = Run parser2 input index
                match result2 with
                | Error index -> Error (index)

                | Success (value, index) -> Success (value, index)

            | Success (value, index) -> Success (value, index)
        Parser innerFn
    let (<|>) = Either

    let Lazy (parser1: Parser<'a>) (parser2: Lazy<Parser<'a>>) =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser1 input index

            match result1 with
            | Error index -> 
                let result2 = Run (parser2.Force()) input index
                match result2 with
                | Error index -> Error (index)

                | Success (value, index) -> Success (value, index)

            | Success (value, index) -> Success (value, index)
        Parser innerFn
    let (?<|>) = Lazy

    let Transform (parser: Parser<'T>) fn =
        let innerFn (input: string) (index: int) =
            let result1 = Run parser input index

            match result1 with
            | Error index -> Error (index)
            | Success (value, index) -> 
                let mapped = fn value
                Success (mapped, index)
        Parser innerFn
    let (|>>) = Transform

    let Any (parsers: Parser<'T> seq) =
        parsers |> Seq.reduce(<|>)

    let AnyOf (c: char seq) =
        Any(c |> Seq.map (fun c -> CharParser c))

    let Optional (parser: Parser<'T>) = 
        let innerFn (input: string) (index: int) =
            match Run parser input index with
            | Error index -> Success (None, index)
            | Success (value, index) -> Success (Some value, index)
        Parser innerFn

    let Sequence (parsers: (Parser<'T>) seq) =
        parsers 
        |> Seq.map (fun parser -> parser |>> List.singleton)
        |> Seq.reduce (fun p1 p2 -> p1 .>>. p2 |>> (fun (v1, v2) -> v1 @ v2))
    
    let Literal (literal: string) (value: 'V) =
        literal
        |> Seq.map CharParser
        |> Sequence
        |>> (fun _ -> value)

    let ZeroOrMore (parser: Parser<'T>) = 
        let innerFn (input: string) (index: int) =
            let rec recurse (index: int) (values: 'T list) =
                 match Run parser input index with
                 | Error index -> Success (List.rev values, index)
                 | Success (value, index) ->
                    recurse index (value :: values)
            recurse index [] 
        Parser innerFn

    let OneOrMore (parser: Parser<'T>) = 
        let innerFn (input: string) (index: int) =
            match Run (ZeroOrMore parser) input index with
            | Error index -> Error (index)
            | Success (values: 'T list, remaining_input) -> 
                if (values.IsEmpty) then
                    Error (index)
                else
                    Success (values, remaining_input)
        Parser innerFn
