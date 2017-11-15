type Token =
    | Integer of int
    | Symbol of string
    | Definition of name: string option * tokens: Token list
    | Error of errorType: string * description: string
    
type Stack = Token list

let StackUnderFlow description = Error ("StackUnderFlow", description)

(* Function helpers *)
let oneArg (f: (Token -> Stack -> Stack)) (stack: Token list) =
    match stack with
    | [] -> StackUnderFlow "needs 1" :: stack
    | x::xs -> f x xs
    
let twoArg f (stack: Token list) =
    match stack with
    | [] | [_] -> printfn "STACK UNDERFLOW; needs 2"; stack
    | x1 :: x2 :: xs -> f x1 x2 xs

(* Functions *)
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
        
let pop stack =
    match stack with
    | [] -> StackUnderFlow "needs 1 or more" :: stack
    | x :: xs -> xs
    
let beginDef stack =
    Definition (None, []) :: stack
    
let endDef stack =
    stack
    

(* Parsing *)
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
    

/// Peeks at stack, if error is found then output it then remove it
let cleanErrors stack =
    match stack with
    | Error (errorType, description) :: xs ->
        printfn "ERROR %s %s" errorType description
        xs
    | _ -> stack
    
let parseSymbol (symbol: string) stack =
    let parser = match symbol with
                 | "dup" -> dup2
                 | "add" -> add
                 | "sum" -> sum
                 | "pop" -> pop
                 | ":" -> beginDef
                 | ";" -> endDef
                 | _ -> printfn "Unknown symbol %s" symbol; nop
    parser stack
    |> cleanErrors
    
// parseToken : Token list -> Token -> Token list
let parseToken stack token =
    printfn ">> %A" token
    // If we have a definition on the stack, we're in "definition mode"
    match stack with
    | Definition (None, tokens) :: xs ->
        match token with
        | Symbol s -> Definition (Some(s), tokens) :: xs // set the definition's name
        | _        -> printfn "Can't use definition name %A" token ; stack // unhandled name
    | Definition (Some(name), tokens) :: xs ->
        match token with
        | Symbol ";" -> // Add definition to dict
            printfn "Got definition %A" (List.head stack)
            xs  
        | _ -> // add token to tokens
            Definition (Some(name), token :: tokens) :: xs 
            
    // ...otherwise, perform regular handling
    | _ ->
        match token with
        | Integer i -> Integer i :: stack
        | Definition (name, tokens) -> printfn "consider handling this case..."; stack
        | Error (errorType, description) -> Error (errorType, description) :: stack
        | Symbol s -> parseSymbol s stack
    |? ":: %A"
    
[<EntryPoint>]
let main argv = 
    tokenize ": square dup add ; 4 square"
    |> List.fold parseToken []
    |> printfn "%A"
    0 // return an integer exit code
    
(*

main [| |] |> ignore

*)
