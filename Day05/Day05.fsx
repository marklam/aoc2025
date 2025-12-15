#r "nuget:Unquote"

open System
open Swensen.Unquote

type Range = { First: int64; Last: int64 }
type IngredientDb = { Fresh : Range list; Avaliable : int64 list }

let parse (text : string[]) : IngredientDb =
    let separatorIndex = text |> Array.findIndex (fun line -> String.IsNullOrWhiteSpace line)
    let ranges = text.[0..separatorIndex-1]
    let ids = text.[separatorIndex+1..]

    let freshRanges =
        ranges
        |> Array.map (fun line ->
            let parts = line.Split('-')
            { First = int64 parts[0]; Last = int64 parts[1] })
        |> Array.toList

    let avaliableIngredients =
        ids |> Array.map int64 |> Array.toList

    { Fresh = freshRanges; Avaliable = avaliableIngredients }

let freshIngredients (db : IngredientDb) =
    let isFresh id =
        db.Fresh
        |> List.exists (fun range -> id >= range.First && id <= range.Last)
    
    db.Avaliable
    |> List.filter isFresh

let testData =
    [|
        "3-5"
        "10-14"
        "16-20"
        "12-18"
        ""
        "1"
        "5"
        "8"
        "11"
        "17"
        "32"
    |]

test <@ 
    (testData |> parse |> freshIngredients) = [5; 11; 17]
   @>

let realData =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let realResult =
    realData
    |> parse
    |> freshIngredients
    |> List.length

printfn "Result = %d" realResult

