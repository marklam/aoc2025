open System
open System.IO
open Swensen.Unquote
open System.Collections.Generic

type Location = { X : int; Y : int; Z : int }
type BoxDistance = { From : Location; To : Location; Distance : float }

let parse (lines : string[]) : Location[] =
    lines
    |> Array.map (fun line ->
        let parts = line.Split(',')
        { X = int parts[0]; Y = int parts[1]; Z = int parts[2] }
    )

let measure locations =
    let n = Array.length locations
    let distances = ResizeArray<BoxDistance>(n * n / 2)
    for i = 0 to n - 1 do
        let loc1 = locations[i]
        for j = i + 1 to n - 1 do
            let loc2 = locations[j]
            let dx = float (loc1.X - loc2.X)
            let dy = float (loc1.Y - loc2.Y)
            let dz = float (loc1.Z - loc2.Z)
            let dist = sqrt (dx * dx + dy * dy + dz * dz)
            distances.Add({ From = loc1; To = loc2; Distance = dist })
    distances.ToArray()
    |> Array.sortBy (fun bd -> bd.Distance)
    
let join location1 location2 connections =
    let circuit1 = connections |> Map.tryFind location1
    let circuit2 = connections |> Map.tryFind location2

    let newCircuit = 
        match circuit1, circuit2 with
        | None, None ->
            Set.ofList [ location1; location2 ]
        | Some circuit1, None ->
            circuit1 |> Set.add location2
        | None, Some circuit2 ->
            circuit2 |> Set.add location1
        | Some circuit1, Some circuit2 ->
            Set.union circuit1 circuit2

    (connections, newCircuit)
    ||> Seq.fold (fun acc loc ->
        acc |> Map.add loc newCircuit
    )

let uniqueValues connections =
    connections
    |> Map.values
    |> Seq.distinct

let result connections =
    uniqueValues connections
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (*) 1

let testData = 
    [|
        "162,817,812"
        "57,618,57"
        "906,360,560"
        "592,479,940"
        "352,342,300"
        "466,668,158"
        "542,29,236"
        "431,825,988"
        "739,650,466"
        "52,470,668"
        "216,146,977"
        "819,987,18"
        "117,168,530"
        "805,96,715"
        "346,949,466"
        "970,615,88"
        "941,993,340"
        "862,61,35"
        "984,92,344"
        "425,690,689"
    |]
    |> parse
    |> measure

test <@
    (testData[0] |> fun l -> l.From,l.To) = ({X = 162; Y = 817; Z = 812}, { X = 425; Y = 690; Z = 689 })
    @>

test <@
    (testData[1] |> fun l -> l.From, l.To) = ({ X = 162; Y = 817; Z = 812}, { X = 431; Y = 825; Z = 988 })
    @>

test <@
    (testData[2] |> fun l -> l.From, l.To) = ({ X = 906; Y = 360; Z = 560 }, { X = 805; Y = 96; Z = 715 })
    @>

test <@
    (testData[0] |> fun l -> join l.From l.To Map.empty |> uniqueValues |> List.ofSeq) =
        [ Set.ofList [ { X = 162; Y = 817; Z = 812 }; { X = 425; Y = 690; Z = 689 } ] ]
    @>

test <@
    (testData 
        |> Array.take 2 
        |> Array.fold (fun conns l -> join l.From l.To conns) Map.empty 
        |> uniqueValues 
        |> List.ofSeq) =
        [ Set.ofList [ { X = 162; Y = 817; Z = 812 }; { X = 425; Y = 690; Z = 689 }; { X = 431; Y = 825; Z = 988 } ] ]
    @>

test <@
    (testData 
        |> Array.take 3
        |> Array.fold (fun conns l -> join l.From l.To conns) Map.empty 
        |> uniqueValues 
        |> List.ofSeq) =
        [ 
            Set.ofList [ { X = 162; Y = 817; Z = 812 }; { X = 425; Y = 690; Z = 689 }; { X = 431; Y = 825; Z = 988 } ] 
            Set.ofList [ { X = 906; Y = 360; Z = 560 }; { X = 805; Y = 96; Z = 715 } ]
        ]
    @>

test <@
    (testData 
        |> Array.take 4
        |> Array.fold (fun conns l -> join l.From l.To conns) Map.empty 
        |> uniqueValues 
        |> List.ofSeq) =
        [ 
            Set.ofList [ { X = 162; Y = 817; Z = 812 }; { X = 425; Y = 690; Z = 689 }; { X = 431; Y = 825; Z = 988 } ] 
            Set.ofList [ { X = 906; Y = 360; Z = 560 }; { X = 805; Y = 96; Z = 715 } ]
        ]
    @>

test <@
    (testData 
        |> Array.take 10 
        |> Array.fold (fun connections dist -> join dist.From dist.To connections) Map.empty 
        |> uniqueValues 
        |> Seq.length) = 11 - 7
    @>

test <@
    (testData 
        |> Array.take 10 
        |> Array.fold (fun connections dist -> join dist.From dist.To connections ) Map.empty 
        |> result) = 40
    @>

let realData =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> parse
    |> measure

let realResult =
    realData
    |> Array.take 1000
    |> Array.fold (fun connections dist -> join dist.From dist.To connections) Map.empty
    |> result

printfn "Result : %d" realResult