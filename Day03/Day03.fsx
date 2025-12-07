#r "nuget:Unquote"

open System
open Swensen.Unquote

let joltage (bank : string) =
    let mutable maxFirstDigit = -1
    let mutable maxSecondDigit = -1
    let mutable pos = 0
    let x = bank.AsSpan()
    
    for i = 0 to x.Length - 2 do
        let c = int (x[i] - '0')
        if c > maxFirstDigit then 
            pos <- i
            maxFirstDigit <- c

    for i = pos + 1 to x.Length - 1 do
        let c = int (x[i] - '0')
        if c > maxSecondDigit then 
            maxSecondDigit <- c

    maxFirstDigit * 10 + maxSecondDigit

let testData = 
    [|
        "987654321111111"
        "811111111111119"
        "234234234234278"
        "818181911112111"
    |]

test <@

    [|98; 89; 78; 92|] = (
        testData
        |> Array.map joltage
        )
    @>

let realData =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let realResult =
    realData
    |> Array.map joltage
    |> Array.sum