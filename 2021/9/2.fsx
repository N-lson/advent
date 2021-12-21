open System.IO

type Point = {
    Row: int
    Col: int
}

let split (char:char) (str:string) = str.Split char

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun line -> Seq.toArray line |> Array.map (fun x -> int x - int '0'))
    |> array2D

let lowPoints (arr:int [,]) = 
    arr |> Array2D.mapi (fun r c v -> 
            let neighbors = [if r > 0 then arr.[r-1,c]
                             if r < Array2D.length1 arr - 1 then arr.[r+1,c]
                             if c > 0 then arr.[r,c-1]
                             if c < Array2D.length2 arr - 1 then arr.[r,c+1]]
            if List.forall (fun x -> x > v) neighbors then {Row = r; Col = c} else {Row = -1; Col = -1}
            )
        |> Seq.cast<Point>
        |> Seq.filter (fun point -> point.Row <> -1 && point.Col <> -1)
        |> Seq.toList

let rec getBasinPoints curPoint pointsSoFar (arr:int [,]) =
    let notNineNeighbors = [if curPoint.Row > 0 then {Row = curPoint.Row-1; Col = curPoint.Col}
                            if curPoint.Row < Array2D.length1 arr - 1 then {Row = curPoint.Row+1; Col = curPoint.Col}
                            if curPoint.Col > 0 then {Row = curPoint.Row; Col = curPoint.Col-1}
                            if curPoint.Col < Array2D.length2 arr - 1 then {Row = curPoint.Row; Col = curPoint.Col+1}] |> List.filter (fun point -> arr.[point.Row, point.Col] <> 9 && not (List.contains point pointsSoFar))
    List.fold (fun triedPoints point -> getBasinPoints point triedPoints arr) (pointsSoFar @ notNineNeighbors) notNineNeighbors

let res =
    lowPoints input
    |> List.map (fun point -> getBasinPoints point [point] input)
    |> List.map (fun basinPoints -> basinPoints.Length)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

printfn "Result: %A" res