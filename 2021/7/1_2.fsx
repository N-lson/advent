open System.IO

let split (char:char) (str:string) = str.Split char

let input = File.ReadAllLines "input.txt" |> Array.head |> split ',' |> Array.toList |> List.map int

let minCrab = List.min input
let maxCrab = List.max input
let possiblePositions = [minCrab..maxCrab]

let part1Res =
    possiblePositions
    |> List.fold (fun lowestSteps currentPos -> 
        let cur = List.fold (fun steps crabPos -> steps + abs(crabPos - currentPos)) 0 input
        if cur < lowestSteps then cur else lowestSteps
        ) System.Int32.MaxValue

let sumTo n = if n = 0 then 0 else [1..n] |> List.reduce (fun sum x -> sum + x)
let part2Res =
    possiblePositions
    |> List.fold (fun lowestSteps currentPos -> 
        let cur = List.fold (fun steps crabPos -> steps + sumTo (abs(crabPos - currentPos))) 0 input
        if cur < lowestSteps then cur else lowestSteps
        ) System.Int32.MaxValue

printfn "Part 1 Result: %d" part1Res
printfn "Part 2 Result: %d" part2Res