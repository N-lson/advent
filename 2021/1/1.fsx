open System.IO

let input = File.ReadAllLines "input.txt" |> Array.map int |> Array.toList

let increasesPairwise list =
    list
    |> List.pairwise
    |> List.filter (fun (a, b) -> b > a)
    |> List.length

let rec increasesRecursively list =
    match list with
    | [] -> 0
    | [_] -> 0
    | x::y::xs -> if y > x then 1 + increasesRecursively(y::xs) else increasesRecursively(y::xs)

// Can use pairwise or recursive
let ans = input |> increasesPairwise

printfn "Result: %d" ans