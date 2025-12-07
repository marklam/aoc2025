#r "nuget:Unquote"

open System
open Swensen.Unquote

let parseRanges (ranges:string) =
    [
        for range in ranges.Split(",") do
            let ends = range.Split("-")
            yield (int64 ends[0], int64 ends[1])
    ]

let isInvalid (x : int64) =
    let s = string x
    if s.Length % 2 <> 0 then 
        false
    else
        let x = s.AsSpan()
        let a = x.Slice(0, x.Length / 2)
        let b = x.Slice(x.Length / 2, x.Length / 2)
        a.Equals(b, StringComparison.Ordinal)

let invalidNumbersInRange test (a, b) =
    [
        for x = a to b do
            if test x then yield x
    ]

let testData = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

test <@
    [11L; 22L; 99L; 1010L; 1188511885L; 222222L; 446446L; 38593859L] = (
        testData
        |> parseRanges
        |> List.collect (fun (a, b) -> invalidNumbersInRange isInvalid (a, b))
    )
    @>

let realData = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
let realResult =
    realData
    |> parseRanges
    |> List.collect (fun (a, b) -> invalidNumbersInRange isInvalid (a, b))
    |> List.sum

printfn "Result: %d" realResult

let isInvalid2 (x : int64) =
    let s = string x
    [1 .. s.Length-1]
    |> List.exists (fun len ->
        if len > 1 && s.Length % len <> 0 then 
            false
        else
            let allMatch =
                [len .. len .. (s.Length - len)]
                |> List.forall (fun o -> 
                    let x = s.AsSpan()
                    let a = x.Slice(0, len)
                    a.Equals(x.Slice(o, len), StringComparison.Ordinal)
                )
            allMatch
    )

test <@
    [11L; 22L; 99L; 111L; 999L; 1010L; 1188511885L; 222222L; 446446L; 38593859L; 565656L; 824824824L; 2121212121L] = (
        testData
        |> parseRanges
        |> List.collect (fun (a, b) -> invalidNumbersInRange isInvalid2 (a, b))
    )
    @>

let realResult2 =
    realData
    |> parseRanges
    |> List.collect (fun (a, b) -> invalidNumbersInRange isInvalid2 (a, b))
    |> List.sum

printfn "Result for part 2: %d" realResult2