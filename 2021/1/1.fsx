open System.IO

let input = File.ReadLines "input.txt"

let rec increases list =
    match list with
    | [] -> 0
    | [_] -> 0
    | x::y::xs -> if y > x then 1 + increases(y::xs) else increases(y::xs)

let ans = input |> Seq.toList |> increases

printfn "Result: %d" ans