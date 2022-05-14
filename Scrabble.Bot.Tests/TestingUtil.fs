module internal Scrabble.Bot.Tests.TestingUtil

open Parser

let readLines filePath = System.IO.File.ReadLines(filePath)

let defaultWords = [ "CABLE"; "CARE"; "ABLE" ]

let getLookuptable pieces =
    pieces
    |> List.mapi (fun id tile ->
        match Set.count tile with
        | 0
        | 1 -> Some(tile |> Set.toSeq |> Seq.head |> fst, uint32 id)
        | _ -> None)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> Map.ofList

let getHandId (c: char) lookupTable = Map.tryFind c lookupTable |> Option.get

let internal mockState (startingHand: uint32 list) (words: string list) : State.state =
    let board = mkBoard (ScrabbleUtil.StandardBoard.standardBoard ())

    let dictAPI =
        Some(Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse)

    let dict = (ScrabbleUtil.Dictionary.mkDict words dictAPI) true

    let playerId = 0u
    let hand = MultiSet.ofList startingHand
    let players = []
    let playerTurn = 0u

    State.mkState board dict playerId hand players playerTurn

let sort (a: ScrabbleUtil.tile) (b: ScrabbleUtil.tile) =
    let aux tile = tile |> Set.toSeq |> Seq.head |> fst

    if a |> Set.count > 1 then -1
    else if b |> Set.count > 1 then 1
    else (aux a).CompareTo(aux b)

let getSortedAndPieces () =
    let tiles = ScrabbleUtil.English.tiles 1u |> List.map fst

    let sorted = List.sortWith sort tiles

    let pieces =
        sorted
        |> List.mapi (fun i tile -> (uint32 i, tile))
        |> Map.ofList

    (sorted, pieces)
