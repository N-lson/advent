open System.IO

let input =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map (fun line -> Seq.toArray line |> Array.toList)

let rec incomplete stack (chars:char list) =
    if chars.IsEmpty then stack else
        match chars.Head with
        | '(' | '[' | '{' | '<' -> incomplete (chars.Head::stack) chars.Tail
        | ')' -> if stack.Head <> '(' then [] else incomplete stack.Tail chars.Tail
        | ']' -> if stack.Head <> '[' then [] else incomplete stack.Tail chars.Tail
        | '}' -> if stack.Head <> '{' then [] else incomplete stack.Tail chars.Tail
        | '>' -> if stack.Head <> '<' then [] else incomplete stack.Tail chars.Tail
        | _ -> failwith "Unknown character"

let res =
    input
    |> List.map (incomplete [])
    |> List.filter (fun stack -> stack <> [])
    |> List.map (fun line ->
        List.fold (fun (total:int64) c ->
            match c with
            | '(' -> total * 5L + 1L
            | '[' -> total * 5L + 2L
            | '{' -> total * 5L + 3L
            | '<' -> total * 5L + 4L
            | _ -> failwith "Unknown character"
        ) 0L line)
    |> List.sort
    |> List.toArray

printfn "Result: %A" res.[(res.Length-1)/2]
