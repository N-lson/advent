open System.IO

let split (str:string) = str.Split [|' '|]
let command list = list |> List.head
let units list = list |> List.tail |> List.head |> int

type Length = int
type Depth = int
type Aim = int

type Course = Length * Depth * Aim

let input = File.ReadAllLines "input.txt"

let commands =
    input
    |> Array.map (fun s -> split s |> Array.toList)
    |> Array.toList

let updateCourse command value course : Course =
    let l, d, a = course
    match command with
    | "forward" -> (l + value, d + a * value, a)
    | "up" -> (l, d, a - value)
    | "down" -> (l, d, a + value)
    | _ -> (0, 0, 0)

let rec calculateCourse commands (course:Course) =
    match commands with
    | [] -> course
    | [x] -> updateCourse (command x) (units x) course
    | x::xs -> calculateCourse xs (updateCourse (command x) (units x) course)

let finalLength, finalDepth, _ = calculateCourse commands (0,0,0)
let total = finalLength * finalDepth

printfn "Result: %d" total
