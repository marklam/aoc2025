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

let columns2 (text : string[]) =
    let rec split acc (text : string[]) offset =
        let befores = 
            Array.init offset (fun i -> 
                text 
                |> Array.choose (fun line -> 
                    let ch = line.[i]
                    if ch = ' ' then 
                        None 
                    else 
                        Some ch)
                |> String
            )
        if text[0].Length = offset then
            (befores :: acc)
        elif text |> Array.forall (fun line -> line[offset] = ' ') then
            let afters = 
                text 
                |> Array.map (fun line -> line.[offset+1..])
            split (befores :: acc) afters 0
        else
            split acc text (offset + 1)

    split [] text 0
    |> List.rev

let calcs2 (columns : string[] list) =
    columns
    |> List.map (fun cols ->
        let opChar = cols[0] |> Seq.last
        let op =
            match opChar with
            | '+' -> Add
            | '*' -> Multiply
            | _ -> failwith "Unknown operation"
        let values =
            cols
            |> Array.map (fun col ->
                col.TrimEnd [|'+'; '*'|]
                |> int
            )
        { Op = op; Values = values }
    )
    |> List.toArray

test <@
    (testData |> columns2) =
        [
            [| "1*"; "24"; "356" |];
            [| "369+"; "248"; "8" |];
            [| "32*"; "581"; "175" |];
            [| "623+"; "431"; "4" |];
        ]
        @>

test <@
    (testData |> columns2 |> calcs2) =
        [|
            { Op = Multiply; Values = [|1; 24; 356|] }
            { Op = Add; Values = [|369; 248; 8|] }
            { Op = Multiply; Values = [|32; 581; 175|] }
            { Op = Add; Values = [|623; 431; 4|] }
        |]
    @>

test <@
    (testData |> columns2 |> calcs2 |> doHomework) = 3263827UL
    @>

let realResult2 =
    realData
    |> columns2
    |> calcs2
    |> doHomework

printfn "Result2 = %d" realResult2