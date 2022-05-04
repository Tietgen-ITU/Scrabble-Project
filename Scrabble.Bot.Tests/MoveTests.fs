module Scrabble.Bot.Tests.MoveTests

open NUnit.Framework

open Moves
open Parser
open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let words = [ "CABLE"; "CARE"; "ABLE" ]

let getLookuptable pieces =
    List.mapi
        (fun id tile ->
            if Set.count tile > 1 then
                None
            else
                Some(tile |> Set.toSeq |> Seq.head |> fst, uint32 id))
        pieces
    |> List.filter Option.isSome
    |> List.map Option.get
    |> Map.ofList

let getHandId (c: char) lookupTable = Map.tryFind c lookupTable |> Option.get

let internal mockState (startingHand: uint32 list) : State.state =
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

[<Test>]
let playWord () =
    let tiles = ScrabbleUtil.English.tiles 1u |> List.map fst

    let sorted = List.sortWith sort tiles

    let pieces =
        sorted
        |> List.mapi (fun i tile -> (uint32 i, tile))
        |> Map.ofList


    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    getHandId 'R' lookupTable
                    getHandId 'E' lookupTable ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), ('C', 1))
                          ((0, 1), ('A', 1))
                          ((0, 2), ('B', 1))
                          ((0, 3), ('L', 1))
                          ((0, 4), ('E', 1)) ])
            state

    let res = getNextMove state pieces
    Assert.AreEqual(((1, 0), 'A'), res |> List.item 0)
    Assert.AreEqual(((2, 0), 'R'), res |> List.item 1)
    Assert.AreEqual(((3, 0), 'E'), res |> List.item 2)
