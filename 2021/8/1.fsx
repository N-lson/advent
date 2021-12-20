open System.IO

let split (char:char) (str:string) = str.Split char

let input =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map (fun line ->
        line.Split " | "
        |> Array.map (fun part ->
            split ' ' part
            |> Array.toList
        )
        |> Array.toList
    )

let res =
    input
    |> List.fold (fun count (line:string list list) -> 
        let lineCount =
            line.Tail
            |> List.head
            |> List.filter (fun (code:string) -> code.Length = 2 || code.Length = 4 || code.Length = 3 || code.Length = 7)
            |> List.length
        lineCount + count
        ) 0

printfn "Result: %d" res