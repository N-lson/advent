open System.IO

let split (char:char) (str:string) = str.Split char

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun line -> Seq.toArray line |> Array.map (fun x -> int x - int '0'))
    |> array2D

printfn "%A" input

let lowPoints (arr:int [,]) = 
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr)
        (fun r c ->
            let curPoint = arr.[r,c]
            let neighbors = [if r > 0 then arr.[r-1,c]
                             if r < Array2D.length1 arr - 1 then arr.[r+1,c]
                             if c > 0 then arr.[r,c-1]
                             if c < Array2D.length2 arr - 1 then arr.[r,c+1]]
            if List.forall (fun x -> x > curPoint) neighbors then curPoint+1 else 0
        )

let lowPointsMap = lowPoints input
printfn "Result: %A" lowPointsMap
let lowPointsSum =
    lowPointsMap
    |> Seq.cast<int>
    |> Seq.sum

printfn "Result: %d" lowPointsSum