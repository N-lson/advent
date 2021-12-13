open System.IO

let split (str:string) = str.Split [|' '|]
let command list = list |> Array.head
let units list = list |> Array.tail |> Array.head |> int

let input = File.ReadAllLines "input.txt"

let commands =
    input
    |> Array.map (fun s -> split s)

let totalForCommand c =
    commands
    |> Array.filter (fun x -> command x = c)
    |> Array.fold (fun prev x -> units x + prev) 0

let total = totalForCommand "forward" * (totalForCommand "down" - totalForCommand "up")
printfn "Result: %d" total
