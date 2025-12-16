open System
open System.IO
open Swensen.Unquote
open System.Collections.Generic

type Cell = | Entry | Space | Splitter

let parse (line : string) =
    line.ToCharArray()
    |> Array.map (fun c ->
        match c with
        | 'S' -> Entry
        | '^' -> Splitter
        | _ -> Space
    )

type State = { SplitCount : int; Positions : Set<int> }

let initialState line =
    { SplitCount = 0; Positions = Set.singleton (line |> Array.findIndex (fun cell -> cell = Entry)) }

let nextState state (line : Cell[]) =
    let newPositions = 
        state.Positions
        |> Seq.collect (fun pos ->
            match line[pos] with
            | Entry ->
                failwith "Should not happen"
            | Space ->
                Seq.singleton pos
            | Splitter ->
                [ pos - 1; pos + 1 ]
        )
        |> Set.ofSeq

    let splits =
        state.Positions
        |> Set.filter (fun pos -> line[pos] = Splitter)
        |> Set.count
    
    { SplitCount = state.SplitCount + splits; Positions = newPositions }

let runSimulation (lines : string[]) =
    let parsedLines = lines |> Array.map parse
    let initial = initialState parsedLines[0]

    (initial, parsedLines[1..])
    ||> Array.fold nextState

let testData =   
    [|
        ".......S......."
        "..............."
        ".......^......."
        "..............."
        "......^.^......"
        "..............."
        ".....^.^.^....."
        "..............."
        "....^.^...^...."
        "..............."
        "...^.^...^.^..."
        "..............."
        "..^...^.....^.."
        "..............."
        ".^.^.^.^.^...^."
        "..............."
    |]

test <@
    (testData |> runSimulation |> _.SplitCount) = 21
    @>

let realData =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let result = realData |> runSimulation
printfn "Result: %d" result.SplitCount

type [<Struct>] Timeline = { XPosition : int; YPosition : int}


let runQuantumSimulation (lines : string[]) =
    let lines = lines |> Array.map parse
    let initialPosition = lines[0] |> Array.findIndex (fun cell -> cell = Entry)
    let initialTimeline = { XPosition = initialPosition; YPosition = 1}

    let walkedCache = Dictionary()

    let rec runQuantumSimulationInner currentTimeline =
        match walkedCache.TryGetValue(currentTimeline) with
        | true, paths -> paths
        | false, _ ->
            let paths =
                if currentTimeline.YPosition = lines.Length then
                    1L
                else
                    let line = lines[currentTimeline.YPosition]
                    match line[currentTimeline.XPosition] with
                    | Entry ->
                        failwith "Should not happen"
                    | Space ->
                        runQuantumSimulationInner { currentTimeline with YPosition = currentTimeline.YPosition+1 }
                    | Splitter ->
                        let leftTimeline = { XPosition = currentTimeline.XPosition - 1; YPosition = currentTimeline.YPosition + 1 }
                        let rightTimeline = { XPosition = currentTimeline.XPosition + 1; YPosition = currentTimeline.YPosition + 1 }
                        runQuantumSimulationInner leftTimeline + runQuantumSimulationInner rightTimeline
            walkedCache.[currentTimeline] <- paths
            paths

    runQuantumSimulationInner initialTimeline

test <@
    (testData |> runQuantumSimulation) = 40
    @>

let realQuantumResult = realData |> runQuantumSimulation
printfn "Quantum Result: %d" realQuantumResult
