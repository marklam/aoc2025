#r "nuget:Unquote"

open System
open Swensen.Unquote

let columns (text : string[]) =
    text
    |> Array.map (
        fun line ->
            line.Trim(' ').Split(' ', StringSplitOptions.RemoveEmptyEntries)
    )

type Operation =
    | Add
    | Multiply

type Calculation = { Op : Operation; Values : int[] }

let transpose (columns : string[][]) =
    let calcCount = columns[0].Length
    let opRow = columns |> Array.last
    Array.init calcCount (fun col ->
        let op = 
            match opRow[col] with
            | "+" -> Add
            | "*" -> Multiply
            | _ -> failwith "Unknown operation"
        
        { Op = op; Values = columns |> Array.take (columns.Length - 1) |> Array.map (fun row -> int row[col]) }
    )

let doHomework (calcs : Calculation[]) =
    calcs
    |> Array.map (fun calc ->
        match calc.Op with
        | Add -> calc.Values |> Array.map uint64 |>  Array.sum
        | Multiply -> calc.Values |> Array.map uint64 |> Array.fold (*) 1UL
    )
    |> Array.sum

let testData =
    [|
        "123 328  51 64 "
        " 45 64  387 23 "
        "  6 98  215 314"
        "*   +   *   +  "
    |]

test <@
    (testData |> columns |> transpose) = 
        [|
            { Op = Multiply; Values = [|123; 45; 6|] };
            { Op = Add; Values = [|328; 64; 98|] };
            { Op = Multiply; Values = [|51; 387; 215|] };
            { Op = Add; Values = [|64; 23; 314|] };
        |]
    @>

test <@
    (testData |> columns |> transpose |> doHomework) = 4277556UL
    @>

let realData =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let realResult =
    realData
    |> columns
    |> transpose
    |> doHomework

printfn "Result = %d" realResult