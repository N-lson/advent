open System.IO

let input = File.ReadAllLines "input.txt" |> Array.map int |> Array.toList

let increasesWindowed list =
    list
    |> List.windowed 3
    |> List.pairwise
    |> List.filter (fun (a, b) -> List.sum b > List.sum a)
    |> List.length

let rec increasesRecursively list =
    match list with
    | [] -> 0
    | [_] -> 0
    | _::_::[] -> 0
    | _::_::_::[] -> 0
    | a::b::c::d::xs -> if (b + c + d) > (a + b + c) then 1 + increasesRecursively(b::c::d::xs) else increasesRecursively(b::c::d::xs)

let ans = input |> increasesWindowed

printfn "Result: %d" ans