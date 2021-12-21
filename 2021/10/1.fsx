open System.IO

let input =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map (fun line -> Seq.toArray line |> Array.toList)

let rec corrupted stack (chars:char list) =
    if chars.IsEmpty then '0' else
        match chars.Head with
        | '(' | '[' | '{' | '<' -> corrupted (chars.Head::stack) chars.Tail
        | ')' -> if stack.Head <> '(' then ')' else corrupted stack.Tail chars.Tail
        | ']' -> if stack.Head <> '[' then ']' else corrupted stack.Tail chars.Tail
        | '}' -> if stack.Head <> '{' then '}' else corrupted stack.Tail chars.Tail
        | '>' -> if stack.Head <> '<' then '>' else corrupted stack.Tail chars.Tail
        | _ -> failwith "Unknown character"

let res =
    input
    |> List.map (corrupted [])
    |> List.filter (fun c -> c <> '0')
    |> List.fold (fun total c ->
        match c with
        | ')' -> total + 3
        | ']' -> total + 57
        | '}' -> total + 1197
        | '>' -> total + 25137
        | _ -> failwith "Unknown character") 0

printfn "Result: %d" res

// let counts = input |> List.map (fun line -> Seq.toArray line |> Array.toList |> List.countBy id |> Map.ofList)
// printfn "%A" counts