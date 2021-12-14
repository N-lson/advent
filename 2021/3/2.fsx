open System.IO
open System

let input = File.ReadAllLines "input.txt"
let sorted = input |> Array.sort

let rec oxygen depth (arr:string[]) = 
    let bit = arr.[(arr.Length)/2].[depth]
    let filtered = Array.filter (fun (x:string) -> x.[depth] = bit) arr
    if filtered.Length = 1 then Array.head filtered else oxygen (depth + 1) filtered

let rec co2 depth (arr:string[]) = 
    let bit = if arr.[(arr.Length)/2].[depth] = '0' then '1' else '0'
    let filtered = Array.filter (fun (x:string) -> x.[depth] = bit) arr
    printfn "%A" filtered
    if filtered.Length = 1 then Array.head filtered else co2 (depth + 1) filtered

let oxygenString = oxygen 0 sorted
let co2String = co2 0 sorted

let lifeSupport = Convert.ToInt32(oxygenString, 2) * Convert.ToInt32(co2String, 2)

printfn "Result: %d" lifeSupport