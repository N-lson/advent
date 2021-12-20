open System
open System.IO

type Digit = {
    Number: string
    Pattern: string
}

let split (char:char) (str:string) = str.Split char

let input =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map (fun line ->
        line.Split " | "
        |> Array.map (fun part ->
            split ' ' part
            |> Array.map (fun (pattern:string) ->
                pattern
                |> Seq.sort
                |> String.Concat
            )
            |> Array.toList
        )
        |> Array.toList
    )

let patternIsSubsetOf pattern otherPattern = Set.ofSeq(pattern).IsSubsetOf (Set.ofSeq(otherPattern))
let patternWithLength length (pattern:string) = pattern.Length = length
let patternNotIn (list:string list) pattern = not (List.contains pattern list)

let calculateDigitsForLine (patterns:string list) =
    let one = { Number = "1"; Pattern = List.find (patternWithLength 2) patterns }
    let four = { Number = "4"; Pattern = List.find (patternWithLength 4) patterns }
    let seven = { Number = "7"; Pattern = List.find (patternWithLength 3) patterns }
    let eight = { Number = "8"; Pattern = List.find (patternWithLength 7) patterns }

    let codesNotInCommonWithOne = List.filter (fun (pattern:string) -> not (patternIsSubsetOf one.Pattern pattern)) patterns
    let six = { Number = "6"; Pattern = List.find (patternWithLength 6) (codesNotInCommonWithOne) }
    let five = { Number = "5"; Pattern = List.find (fun (pattern:string) -> patternIsSubsetOf pattern six.Pattern && pattern <> six.Pattern) (codesNotInCommonWithOne) }
    let two = { Number = "2"; Pattern = List.find (patternNotIn [six.Pattern; five.Pattern]) (codesNotInCommonWithOne) }

    let codesOfFiveLength = List.filter (patternWithLength 5) patterns
    let three = { Number = "3"; Pattern = List.find (patternNotIn [two.Pattern; five.Pattern]) (codesOfFiveLength) }

    let codesInCommonWithFour = List.filter (patternIsSubsetOf four.Pattern) patterns
    let nine = { Number = "9"; Pattern = List.find (patternNotIn [eight.Pattern; four.Pattern]) codesInCommonWithFour }

    let codesOfSixLength = List.filter (patternWithLength 6) patterns
    let zero = { Number = "0"; Pattern = List.find (patternNotIn [six.Pattern; nine.Pattern]) codesOfSixLength }
    [zero;one;two;three;four;five;six;seven;eight;nine]

let calculateCode patterns digits =
    patterns
    |> List.map (fun pattern -> (List.find (fun x -> x.Pattern = pattern) digits).Number)
    |> String.Concat
    |> int

let res =
    input
    |> List.map (fun line -> calculateDigitsForLine line.Head |> calculateCode line.Tail.Head)
    |> List.sum

printfn "Result: %d" res

// let res = input |> List.fold (fun count (line:string list list) -> 
//     let lineCount = line.Tail |> List.head |> List.filter (fun (code:string) -> code.Length = onePatternLength || code.Length = fourPatternLength || code.Length = sevenPatternLength || code.Length = eightPatternLength) |> List.length
//     lineCount + count) 0

// printfn "Part 1 Result: %d" res

