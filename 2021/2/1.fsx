open System.IO

let split (str:string) = str.Split [|' '|]

type Position = { 
    Horizontal: int
    Depth: int
}

let input = File.ReadAllLines "input.txt"

let commands =
    input
    |> Array.map (fun s -> split s)

let updatePosition command position =
    match command with
    | [| "forward"; value |] -> {Horizontal = position.Horizontal + int value; Depth = position.Depth}
    | [| "up"; value |] -> {Horizontal = position.Horizontal; Depth = position.Depth - int value}
    | [| "down"; value |] -> {Horizontal = position.Horizontal; Depth = position.Depth + int value}
    | _ -> failwith "Unknown command"

let calculatePosition commands =
    commands
    |> Array.fold (fun pos x -> updatePosition x pos) {Horizontal = 0; Depth = 0}

let finalPosition = calculatePosition commands
let total = finalPosition.Horizontal * finalPosition.Depth
printfn "Result: %d" total
