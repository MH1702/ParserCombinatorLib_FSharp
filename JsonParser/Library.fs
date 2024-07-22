module Json

open ParserCombinatorLib2.Main
open System

type JsonValue =
    | JsonString of string
    | JsonNumber of float
    | JsonObject of Map<string, JsonValue>
    | JsonArray of JsonValue list
    | JsonBool of bool
    | JsonNull

// null
let null_parser = Literal "null" JsonNull

// bools
let false_parser = Literal "false" (JsonBool(false))
let true_parser = Literal "true" (JsonBool(true))
let bool_parser = (false_parser <|> true_parser)

// numbers
let digit_parser = AnyOf(['0'..'9'])
let non_zero_digit_parser = AnyOf(['1'..'9'])
let integer_digits_parser = (
    ((CharParser '0' |>> List.singleton) 
    <|> (
        ((non_zero_digit_parser |>> List.singleton) .>>. ZeroOrMore(digit_parser))
        |>> (fun (a, b) -> a @ b)
    )) 
)
let integer_parser = (
    (Optional (CharParser '-' |>> List.singleton) .>>. integer_digits_parser) 
    |>> (fun (a, b) -> defaultArg a [] @ b)
)
let fraction_parser = (
    (CharParser '.' .>>. OneOrMore(digit_parser)) 
    |>> (fun (a, b) -> [a] @ b)
)
let sign_parser = (CharParser '+' <|> CharParser '-');
let exponent_parser = (
    ((CharParser 'e' <|> CharParser 'E') .>>. sign_parser .>>. OneOrMore(digit_parser)) 
    |>> (fun ((a, b), c) -> [a] @ [b] @ c)
)
let number_parser = (
    (integer_parser .>>. Optional fraction_parser .>>. Optional exponent_parser)
    |>> (fun ((a, b), c) -> String.Concat(a @ defaultArg b [] @ defaultArg c []))
    |>> (fun x -> JsonNumber(Double.Parse(x, Globalization.CultureInfo.InvariantCulture)))
)

// strings
let whitespace_parser = (
    ZeroOrMore(
        Any([
            (CharParser '\u0020'); 
            (CharParser '\u000A'); 
            (CharParser '\u000D'); 
            (CharParser '\u0009');
        ])
    ) |>> String.Concat
)
let hex_character_parser = AnyOf(['A'..'F']) <|> AnyOf(['a'..'f']) <|> digit_parser
let character_parser = 
    (Satisfies(fun c -> not(Char.IsControl(c) || c = '\\' || c = '"')) 
    |>> String.Concat)
    <|> 
    (((CharParser '\\' .>>. Any([
        CharParser '"'; 
        CharParser '\\'; 
        CharParser '/'; 
        CharParser 'b'; 
        CharParser 'f'; 
        CharParser 'n';
        CharParser 'r';
        CharParser 't';
        CharParser 'u' .>>. hex_character_parser .>>. hex_character_parser .>>. hex_character_parser .>>. hex_character_parser 
        |>> (fun ((((a, b), c), d), e) -> 
             a + b + c + d + e
        );
    ])) 
    |>> String.Concat)
)
let characters_parsers = (
    OneOrMore(character_parser) 
    |>> String.Concat
) 
let string_parser = (
    (CharParser '"' >>. Optional characters_parsers .>> CharParser '"') 
    |>> (fun x -> defaultArg x "")
)
let json_string_parser = (
    string_parser
    |>> (fun x -> JsonString(x))
)


let rec value_parser() = 
    null_parser 
    <|> bool_parser 
    <|> number_parser 
    <|> json_string_parser 
    ?<|> array_parser()
    ?<|> object_parser()

and element_parser() = (whitespace_parser >>. value_parser() .>> whitespace_parser)
and elements_parser() = (
    (element_parser() .>>. ZeroOrMore(CharParser ',' >>. element_parser()))
    |>> (fun (a, b) -> [a] @ b)
)

and array_parser() = lazy (
    (CharParser '[' >>. whitespace_parser >>. Optional (elements_parser()) .>> whitespace_parser .>> CharParser ']')
    |>> (fun x -> JsonArray(defaultArg x []))
)

and member_parser() = whitespace_parser >>. string_parser .>> whitespace_parser .>> CharParser ':' .>>. element_parser();
and members_parser() = (
    (member_parser() .>>. ZeroOrMore(CharParser ',' >>. member_parser()))
    |>> (fun (a, b) -> (
        [a] @ b 
        |> List.map (fun (key, value) -> (key, value))
        |> Map.ofList
    ))
)

and object_parser() = lazy (
    (CharParser '{' >>. whitespace_parser >>. members_parser() .>> whitespace_parser .>> CharParser '}')
    |>> (fun x -> JsonObject(x))
)

and json_parser() = (
    (whitespace_parser >>. value_parser() .>> whitespace_parser)
)

let Parse (json: string) = 
    ParserCombinatorLib2.Main.Parse (json_parser()) json