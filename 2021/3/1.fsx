open System.IO
open System

let combineCounts = List.map2 (+)
let flipBits str = String.map (fun c -> if c = '0' then '1' else '0') str

let input = File.ReadAllLines "input.txt" |> Array.toList

let gammaString =
    input
    |> List.map Seq.toList
    |> List.map (List.map (fun x -> int x - int '0'))
    |> List.reduce (fun counts x -> combineCounts counts x)
    |> List.map (fun x -> if x > input.Length / 2 then "1" else "0")
    |> String.concat ""

let gamma = Convert.ToInt32(gammaString, 2)
let epsilon = Convert.ToInt32(flipBits gammaString, 2)
let powerConsumption = gamma * epsilon

printfn "Result: %d" powerConsumption