type Token =
    | Integer of int
    | Symbol of string
    | Error of errorType: string * description: string
    
type Stack = Token list

let StackUnderFlow description = Error ("StackUnderFlow", description)

(* Parser helpers *)
let oneArg (f: (Token -> Stack -> Stack)) (stack: Token list) =
    match stack with
    | [] -> StackUnderFlow "needs 1" :: stack
    | x::xs -> f x xs
    
let twoArg f (stack: Token list) =
    match stack with
    | [] | [_] -> printfn "STACK UNDERFLOW; needs 2"; stack
    | x1 :: x2 :: xs -> f x1 x2 xs

(* Parsers *)
let dup2 = oneArg (fun x xs -> x :: (x :: xs))
let nop stack = stack
let add = twoArg (fun x1 x2 xs ->
    match (x1, x2) with
    | (Integer i1, Integer i2) -> (Integer (i1 + i2)) :: xs
    | _ -> printfn "WARNING: Did not find two ints"; xs)

let sum (stack: Token list) =
    match stack with
    | [] -> StackUnderFlow "needs 1 or more" :: stack
    | xs ->
        xs
        |> Seq.reduce (fun acc elem ->
            match (acc, elem) with
            | (Integer i1, Integer i2) -> Integer (i1 + i2)
            | _ -> Symbol "error")
        |> List.singleton

let split (splitChar: char) (str: string) = str.Split splitChar
let tryParseInt str =
    match System.Int32.TryParse str with
    | true, num -> Some(num)
    | _ -> None
    
let parseStringIntoToken (str: string) =
    match tryParseInt str with
    | Some(integer) -> Integer integer
    | None -> Symbol str
let tokenize = split ' ' >> Array.toList >> List.map parseStringIntoToken
let tee f x =
    f x |> ignore
    x
let (|?) content str =
    tee (printfn str) content
    
    
let getSymbolParser (symbol: string) =
    printfn ">> %s" symbol
    match symbol with
    | "dup" -> dup2
    | "add" -> add
    | "sum" -> sum
    | _ -> printfn "Unknown symbol %s" symbol; nop
    
// parseToken : Token list -> Token -> Token list
let parseToken stack token =
    let parser = match token with
                   | Integer i -> (fun stack -> Integer i :: stack)
                   | Symbol s -> getSymbolParser s
                   | Error (errorType, description) -> id
    parser stack
    |? ":: %A"
    
[<EntryPoint>]
let main argv = 
    tokenize "sum 3"
    |> List.fold parseToken []
    |> printfn "%A"
    0 // return an integer exit code
    
    
(*

main [| |] |> ignore

*)
