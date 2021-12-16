open System.IO

type Point = {
    X: int
    Y: int
}

type Line = {
    Start: Point
    End: Point
}

let split (char:char) (str:string) = str.Split char

let input = File.ReadAllLines "input.txt"

let lines = 
    input
    |> Array.map (fun line -> line.Split " -> " |> Array.map (fun point -> point.Split "," |> Array.map (fun coord -> int coord)))
    |> Array.map (fun line -> { Start = { X = line.[0].[0]; Y = line.[0].[1] }; End = { X = line.[1].[0]; Y = line.[1].[1]}})
    |> Array.toList

let straightLines = lines |> List.filter (fun line -> line.Start.X = line.End.X || line.Start.Y = line.End.Y)

let rec pointsForHorizontalLine x maxY curY =
    match curY = maxY with
    | true -> [{ X = x; Y = curY }]
    | _ -> { X = x; Y = curY }::pointsForHorizontalLine x maxY (curY + 1)
    
let rec pointsForVerticalLine y maxX curX =
    match curX = maxX with
    | true -> [{ X = curX; Y = y }]
    | _ -> { X = curX; Y = y }::pointsForVerticalLine y maxX (curX + 1)

let rec pointsForDiagonalDownLine maxX curX curY =
    match curX = maxX with
    | true -> [{ X = curX; Y = curY }]
    | _ -> { X = curX; Y = curY }::pointsForDiagonalDownLine maxX (curX + 1) (curY + 1)

let rec pointsForDiagonalUpLine maxX curX curY =
    match curX = maxX with
    | true -> [{ X = curX; Y = curY }]
    | _ -> { X = curX; Y = curY }::pointsForDiagonalUpLine maxX (curX + 1) (curY - 1)

let isHorizontal line = line.Start.X = line.End.X
let isVertical line = line.Start.Y = line.End.Y
let isDiagonalDown line = (line.Start.Y > line.End.Y && line.Start.X > line.End.X) || (line.End.Y > line.Start.Y && line.End.X > line.Start.X)

let convertLineToPoints line =
    let maxY = max line.Start.Y line.End.Y
    let startY = min line.Start.Y line.End.Y
    let maxX = max line.Start.X line.End.X
    let startX = min line.Start.X line.End.X
    if isHorizontal line then
        pointsForHorizontalLine line.Start.X maxY startY
    elif isVertical line then
        pointsForVerticalLine line.Start.Y maxX startX
    elif isDiagonalDown line then
        pointsForDiagonalDownLine maxX startX startY
    else
        pointsForDiagonalUpLine maxX startX maxY


let straightPointsThatOverlap =
    straightLines
    |> List.map convertLineToPoints
    |> List.concat
    |> List.countBy id
    |> List.filter (fun counts -> snd counts > 1)

let allPointsThatOverlap =
    lines
    |> List.map convertLineToPoints
    |> List.concat
    |> List.countBy id
    |> List.filter (fun counts -> snd counts > 1)

printfn "Straight line overlaps: %d" straightPointsThatOverlap.Length
printfn "All line overlaps: %d" allPointsThatOverlap.Length