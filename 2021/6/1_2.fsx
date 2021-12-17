open System.IO

let split (char:char) (str:string) = str.Split char

let input = File.ReadAllLines "input.txt" |> Array.head |> split ',' |> Array.toList |> List.map (fun x -> int x) |> List.countBy id |> List.map (fun (x,y) -> (x,int64 y)) |> Map.ofList

let updateTimers timers =
    let decrementedTimers = Map.fold (fun curMap k v -> Map.add (if k-1 < 0 then 8 else k-1) v curMap) Map.empty timers
    match Map.tryFind 8 decrementedTimers with
    | Some newFish -> Map.change 6 (fun v ->
        match v with
        | Some v -> Some (v + newFish)
        | None -> Some (newFish)) decrementedTimers
    | None -> decrementedTimers

let rec nextDay curTimers curDay maxDays =
    if curDay = maxDays then curTimers else nextDay (updateTimers curTimers) (curDay+1) maxDays

let fishTimersAtLastDayPart1 = nextDay input 0 80
let numFishPart1 = fishTimersAtLastDayPart1 |> Map.fold (fun count _ v -> count + v) 0L

let fishTimersAtLastDayPart2 = nextDay input 0 256
let numFishPart2 = fishTimersAtLastDayPart2 |> Map.fold (fun count _ v -> count + int64 v) 0L

printfn "Part 1 Result: %d" numFishPart1
printfn "Part 2 Result: %d" numFishPart2