#r "nuget:Unquote"

open Swensen.Unquote
open System.IO

type Rotation = | Left | Right
type Instruction = { Direction: Rotation; Steps: int }
let dialSize = 100

let parseInstruction (line: string) : Instruction =
    let direction =
        match line[0] with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwith "Invalid direction"
    let steps = line[1..] |> int
    { Direction = direction; Steps = steps }

let applyInstruction (position: int) instruction =
    let increment =
        match instruction.Direction with
        | Left -> - instruction.Steps
        | Right -> instruction.Steps
    let modulo = (position + increment) % dialSize
    if modulo < 0 then modulo + dialSize else modulo

let stoppingPoints start instructions =
    (start, instructions)
    ||> List.scan applyInstruction

let countZeros positions =
    positions
    |> List.filter ((=) 0)
    |> List.length

let testData = 
    [
        "L68"
        "L30"
        "R48"
        "L5"
        "R60"
        "L55"
        "L1"
        "L99"
        "R14"
        "L82"
    ]

test <@ 
    3 = (
        testData 
        |> List.map parseInstruction 
        |> stoppingPoints 50
        |> countZeros
        )
    @>

let realData =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.toList

let realPassword =
    realData
    |> List.map parseInstruction 
    |> stoppingPoints 50
    |> countZeros

printfn "The real password is %d" realPassword

let applyInstruction2 (position: int) instruction =
    let increment =
        match instruction.Direction with
        | Left -> - instruction.Steps
        | Right -> instruction.Steps
    let newPosition = position + increment

    let modulo = newPosition % dialSize
    if newPosition = 0 then 
        {| NewPosition = modulo; Zeros = 1 |}
    elif newPosition < 0 then 
        if position = 0 then
            {| NewPosition = modulo + dialSize; Zeros = abs (newPosition / dialSize) |}
        else
            {| NewPosition = modulo + dialSize; Zeros = 1 + abs (newPosition / dialSize) |}
    else 
        {| NewPosition = modulo; Zeros = newPosition / dialSize |}

let countZeroCrossings (applications : {| NewPosition : int; Zeros : int|} list) =
    applications
    |> List.sumBy (fun x -> x.Zeros)

let applyInstructions2 start instructions =
    ( {| NewPosition = start; Zeros = 0 |}, instructions)
    ||> List.scan (fun acc instr -> applyInstruction2 acc.NewPosition instr)

test <@ 
    6 = (
        testData 
        |> List.map parseInstruction 
        |> applyInstructions2 50
        |> countZeroCrossings
        )
    @>

let realPassword2 =
    realData
    |> List.map parseInstruction 
    |> applyInstructions2 50
    |> countZeroCrossings

printfn "The real password for part 2 is %d" realPassword2