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

let overlaps (range1 : Range) (range2 : Range) =
    not (range1.Last < range2.First || range1.First > range2.Last)

let merge (range1 : Range) (range2 : Range) =
    { First = min range1.First range2.First; Last = max range1.Last range2.Last }

let rec mergeRanges (ranges : Range list) (range : Range) =
    let overlapping =
        ranges |> List.tryFind (overlaps range)
    
    match overlapping with
    | Some overlapping ->
        let rest = ranges |> List.except [overlapping]
        let merged = merge range overlapping
        mergeRanges rest merged
    | None -> range :: ranges

let itemTotal ranges =
    ranges
    |> List.sumBy (fun range -> range.Last - range.First + 1L)

let result2 =
    realData
    |> parse
    |> _.Fresh 
    |> List.fold mergeRanges []
    |> itemTotal

printfn "Result2 = %d" result2