open System.IO

type Board = {
    Lines: int Set list
    DrawsTaken: int
    WinningNumber: int
}

let split (char:char) (str:string) = str.Split char

let input = File.ReadAllLines "input.txt" |> Array.filter (fun x -> x <> "")

let draws = Array.head input |> split ',' |> Array.toList |> List.map int

let boards =
    Array.tail input
    |> Array.toList
    |> List.map (
        fun row ->
        split ' ' row
        |> Array.toList
        |> List.filter (fun cell -> cell <> "")
        |> List.map int
        )
    |> List.chunkBySize 5
    |> List.map (fun board ->
        let rows = board |> List.map Set.ofList
        let cols = List.transpose board |> List.map Set.ofList
        {
            Lines = rows @ cols
            DrawsTaken = 0
            WinningNumber = 0
        }
    )

let calculateNextBoardState board draw =
    if List.forall (fun (line:int Set) -> not line.IsEmpty) board.Lines then
        {
            Lines = List.map (fun line -> Set.remove draw line) board.Lines
            DrawsTaken = board.DrawsTaken + 1
            WinningNumber = draw
        } else board

let calculateFinalBoardState board =
    List.fold calculateNextBoardState board draws

let getAllRemainingNumbers (lines:int Set list) = List.reduce (fun allSets set -> allSets + set) lines

let sumOfAllRemainingNumbers board =
    board.Lines
    |> getAllRemainingNumbers
    |> Seq.sum

let calculateScore board = sumOfAllRemainingNumbers board * board.WinningNumber

let compareBoards b1 b2 =
    let draws = compare b1.DrawsTaken b2.DrawsTaken
    if draws <> 0 then draws else
    compare (calculateScore b2) (calculateScore b1) // We want the greater score first

let sortedBoards =
    List.map calculateFinalBoardState boards
    |> List.sortWith compareBoards

let winningBoard = sortedBoards |> List.head
let winningScore = calculateScore winningBoard

let losingBoard = sortedBoards |> List.rev |> List.head
let losingScore = calculateScore losingBoard

printfn "Part 1"
printfn "Winning Board: %A" winningBoard
printfn "Winning Score: %d" winningScore

printfn "Part 2"
printfn "Losing Board: %A" losingBoard
printfn "Losing Score: %d" losingScore
