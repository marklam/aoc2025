#r "nuget:Unquote"

open Swensen.Unquote

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
