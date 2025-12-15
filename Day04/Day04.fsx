#r "nuget:Unquote"

open System
open Swensen.Unquote

let rolls (text : string[]) : int[][]=
    text 
    |> Array.map (fun s -> Array.init (s.Length) (fun i -> if s[i] = '@' then 1 else 0))

let pad padding ys =
    let innerLen = ys |> Array.head |> Array.length
    let empty = Array.create padding (Array.zeroCreate (padding * 2 + innerLen))
    Array.concat 
        [|
            empty
            ys |> Array.map (fun x -> Array.concat [| (Array.zeroCreate padding); x; (Array.zeroCreate padding) |])
            empty
        |]

let horizontalSums width padding (xs : int[][]) =
    xs 
    |> Array.map (fun row ->
        let sumRow = Array.zeroCreate width
        for i = 0 to width - 1 do
            let mutable sum = 0
            for j = 0 to padding * 2 do
                sum <- sum + row[i + j]
            sumRow[i] <- sum
        sumRow)

let verticalSums height padding (xs : int[][]) =
    let width = xs[0].Length
    [|
        for i = 0 to height - 1 do
            let sumRow = Array.zeroCreate width
            for j = 0 to width - 1 do
                let mutable sum = 0
                for k = 0 to padding * 2 do
                    sum <- sum + xs[i + k][j]
                sumRow[j] <- sum
            yield sumRow
    |]

let sums padding (rolls : int[][]) =
    let padded = pad padding rolls
    let height = rolls.Length
    let width = rolls[0].Length
    let horizontalSums = horizontalSums width padding padded
    let finalSums = verticalSums height padding horizontalSums
    finalSums

let locationsOf1s rollData =
    rollData 
    |> Array.mapi (fun i row ->
        row
        |> Array.mapi (fun j x -> 
            if x = 1 then
                Some (i, j)
            else
                None)
    )
    |> Array.concat
    |> Array.choose id

let testData = 
    [|
        "..@@.@@@@."
        "@@@.@.@.@@"
        "@@@@@.@.@@"
        "@.@@@@..@."
        "@@.@@@@.@@"
        ".@@@@@@@.@"
        ".@.@.@.@@@"
        "@.@@@.@@@@"
        ".@@@@@@@@."
        "@.@.@@@.@."
    |]

test <@ 
    (rolls [| "@.@"; ".@." |]) = [| [|1;0;1|]; [|0;1;0|] |]
    @>

test <@ 
    (pad 2 [| [|1;0;1|]; [|0;1;0|] |]) = 
        [|
            [|0;0;0;0;0;0;0|]
            [|0;0;0;0;0;0;0|]
            [|0;0;1;0;1;0;0|]
            [|0;0;0;1;0;0;0|]
            [|0;0;0;0;0;0;0|]
            [|0;0;0;0;0;0;0|]
        |]
    @>

test <@
    (horizontalSums 5 1 [| [|0;0;1;0;1;0;0|]; [|0;0;0;1;0;0;0|] |]) =
        [| [|1;1;2;1;1|]; [|0;1;1;1;0|] |]
    @>

test <@
    (verticalSums 2 1 [| [|1;1;2;1;1|]; [|0;1;1;1;0|]; [|1;0;2;3;1|]; [|2;0;0;1;2|] |]) =
        [| [|1+0+1;1+1+0;2+1+2;1+1+3;1+0+1|]; [|0+1+2;1+0+0;1+2+0;1+3+1;0+1+2|] |]
    @>

let testResult  =
    let rollData =
        testData
        |> rolls
    
    let summed =
        rollData
        |> sums 1

    let testLocations =
        locationsOf1s rollData

    testLocations
    |> Array.sumBy (fun (i, j) -> if summed[i][j] < 5 then 1 else 0)

test <@ testResult = 13 @>

let realData =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let realResult =
    let rollData =
        realData
        |> rolls
    
    let summed =
        rollData
        |> sums 1

    let realLocations =
        locationsOf1s rollData 

    realLocations
    |> Array.sumBy (fun (i, j) -> if summed[i][j] < 5 then 1 else 0)

printfn "Result = %d" realResult