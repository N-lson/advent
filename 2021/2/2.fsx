open System.IO

let split (str:string) = str.Split [|' '|]
let command list = list |> List.head
let units list = list |> List.tail |> List.head |> int

type Course = {
    Horizontal: int
    Depth: int
    Aim: int
}

let input = File.ReadAllLines "input.txt"

let commands =
    input
    |> Array.map (fun s -> split s)
    |> Array.toList

let updateCourse command course : Course =
    match command with
    | [| "forward"; value|] -> {Horizontal = course.Horizontal + int value; Depth = course.Depth + course.Aim * int value; Aim = course.Aim}
    | [| "up"; value|] -> {Horizontal = course.Horizontal; Depth = course.Depth; Aim = course.Aim - int value}
    | [| "down"; value|] -> {Horizontal = course.Horizontal; Depth = course.Depth; Aim = course.Aim + int value}
    | _ -> failwith "Unknown command"

let calculateCourse commands =
    commands
    |> List.fold (fun course x -> updateCourse x course) {Horizontal = 0; Depth = 0; Aim = 0}

let finalCourse = calculateCourse commands
let total = finalCourse.Horizontal * finalCourse.Depth

printfn "Result: %d" total
