open System.IO

let split (char:char) (str:string) = str.Split char

let input = File.ReadAllLines "input.txt" |> Array.toList |> List.map (fun line -> line.Split " | " |> Array.map (fun part -> split ' ' part |> Array.toList) |> Array.toList)

let numOneSegments = 2
let numFourSegments = 4
let numSevenSegments = 3
let numEightSegments = 7

let res = input |> List.fold (fun count (line:string list list) -> 
    let lineCount = line.Tail |> List.head |> List.filter (fun (code:string) -> code.Length = numOneSegments || code.Length = numFourSegments || code.Length = numSevenSegments || code.Length = numEightSegments) |> List.length
    lineCount + count) 0

printfn "Part 1 Result: %d" res

