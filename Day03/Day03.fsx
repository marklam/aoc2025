#r "nuget:Unquote"

open System
open Swensen.Unquote

let joltage requiredDigits (bank : string) =
    let x = bank.AsSpan()
    //let mutable remainingDigits = requiredDigits
    let mutable skipsLeft = x.Length - requiredDigits
    let mutable digitsLeft = requiredDigits
    let resultDigits = Array.zeroCreate requiredDigits
    let mutable nextDigitStartPoint = 0
    for i = 0 to requiredDigits-1 do
        let mutable maxNextDigit = '/' // just below 0
        let mutable maxNextDigitPos = -1
        for j = nextDigitStartPoint to nextDigitStartPoint + skipsLeft do
            if x[j] > maxNextDigit then
                maxNextDigit <- x[j]
                maxNextDigitPos <- j
        let skips = maxNextDigitPos - nextDigitStartPoint
        skipsLeft <- skipsLeft - skips
        nextDigitStartPoint <- nextDigitStartPoint + skips + 1
        resultDigits[i] <- maxNextDigit
    int64 (String resultDigits)

let testData = 
    [|
        "987654321111111"
        "811111111111119"
        "234234234234278"
        "818181911112111"
    |]

test <@

    [|98L; 89L; 78L; 92L|] = (
        testData
        |> Array.map (joltage 2)
        )
    @>

let realData =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let realResult =
    realData
    |> Array.map (joltage 2)
    |> Array.sum

printfn "Result = %d" realResult

test <@ 17107L = realResult @>

let realResult2 =
    realData
    |> Array.map (joltage 12)
    |> Array.sum

printfn "Result2 = %d" realResult2
