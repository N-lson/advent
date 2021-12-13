open System.IO

let input = File.ReadLines "input.txt"

let rec increases list =
    match list with
    | [] -> 0
    | [_] -> 0
    | a::b::c::d::xs -> if (b + c + d) > (a + b + c) then 1 + increases(b::c::d::xs) else increases(b::c::d::xs)

let ans = input |> Seq.toList |> increases

printfn "Result: %d" ans