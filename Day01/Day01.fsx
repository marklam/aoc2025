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

let posMod x y =
    let modulo = x % y
    if modulo < 0 then modulo + y else modulo

type ZeroCrossing = { Zeros : int; NewPosition : int }

let applyInstruction2 (position: int) instruction =
    let increment =
        match instruction.Direction with
        | Left -> -1
        | Right -> 1

    let mutable newPosition = position
    let mutable zeros = 0
    for i = 1 to instruction.Steps do
        match newPosition + increment with
        | -1 -> 
            newPosition <- dialSize - 1
        | x when x = dialSize -> 
            newPosition <- 0
        | x -> 
            newPosition <- x

        if newPosition = 0 then
            zeros <- zeros + 1

    { Zeros = zeros; NewPosition = newPosition }

let countZeroCrossings applications =
    applications
    |> List.sumBy (fun x -> x.Zeros)

let applyInstructions2 start instructions =
    ( { NewPosition = start; Zeros = 0 }, instructions)
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