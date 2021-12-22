open System.IO

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun line -> Seq.toArray line |> Array.map (fun x -> int x - int '0'))
    |> array2D

let rec step n flashes (arr:int [,]) =
    if n > 100 then flashes else
        let mutable curArr =
            Array2D.init (Array2D.length1 arr) (Array2D.length2 arr)
                (fun r c ->
                    let updated = arr.[r,c] + 1
                    if updated > 9 then 0 else updated
                )
        while (curArr |> Seq.cast<int> |> Seq.contains 0) do
            curArr <- Array2D.init (Array2D.length1 curArr) (Array2D.length2 curArr)
                (fun r c ->
                    let curPoint = curArr.[r,c]
                    if curPoint = 0 || curPoint = -1 then -1
                    else
                        let neighbors = [if r > 0 then curArr.[r-1,c]
                                         if r > 0 && c > 0 then curArr.[r-1,c-1]
                                         if r < Array2D.length1 arr - 1 then curArr.[r+1,c]
                                         if r < Array2D.length1 arr - 1 && c > 0 then curArr.[r+1,c-1]
                                         if c > 0 then curArr.[r,c-1]
                                         if r > 0 && c < Array2D.length2 arr - 1 then curArr.[r-1,c+1]
                                         if c < Array2D.length2 arr - 1 then curArr.[r,c+1]
                                         if r < Array2D.length1 arr - 1 && c < Array2D.length2 arr - 1 then curArr.[r+1,c+1]]
                        let neighborFlashes = List.fold (fun total neighbor -> if (neighbor = 0) then total + 1 else total) 0 neighbors
                        if curPoint + neighborFlashes > 9 then 0 else curPoint + neighborFlashes
                )
        let nextStepArr =
            Array2D.init (Array2D.length1 curArr) (Array2D.length2 curArr)
                (fun r c ->
                    let point = curArr.[r,c]
                    if point = -1 then 0 else point
                )
        let flashesThisStep = nextStepArr |> Seq.cast<int> |> Seq.filter (fun p -> p = 0) |> Seq.length
        step (n+1) (flashes+flashesThisStep) nextStepArr

let res = step 1 0 input
printfn "%d" res
