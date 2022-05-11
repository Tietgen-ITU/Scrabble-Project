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

let getSortedAndPieces () =
    let tiles = ScrabbleUtil.English.tiles 1u |> List.map fst

    let sorted = List.sortWith sort tiles

    let pieces =
        sorted
        |> List.mapi (fun i tile -> (uint32 i, tile))
        |> Map.ofList

    (sorted, pieces)


[<Test>]
let playWord () =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to supress debug output
    let (sorted, pieces) = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    getHandId 'R' lookupTable
                    getHandId 'E' lookupTable ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = getNextMove state pieces
    Assert.AreEqual(((1, 0), (1u, ('A', 1))), res |> Option.get |> List.item 0)
    Assert.AreEqual(((2, 0), (18u, ('R', 1))), res |> Option.get |> List.item 1)
    Assert.AreEqual(((3, 0), (5u, ('E', 1))), res |> Option.get |> List.item 2)

[<Test>]
let playWordDirect () =
    let (sorted, pieces) = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    getHandId 'R' lookupTable
                    getHandId 'E' lookupTable ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal
    printf "%A\n" res
    Assert.AreEqual(PlayedLetter ((0,0), (3u, ('C', 1))), res |> List.head |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (1u, ('A', 1))), res |> List.head |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (18u, ('R', 1))), res |> List.head |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (5u, ('E', 1))), res |> List.head |> List.item 3)

[<Test>]
let playFirstWord () =
    let (sorted, pieces) = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    getHandId 'C' lookupTable
                    getHandId 'R' lookupTable
                    getHandId 'E' lookupTable ]

    let state = State.placeLetters Seq.empty state

    let res = getNextMove state pieces |> Option.get

    printf "%A\n" res
    Assert.AreEqual(((0, 0), (1u, ('A', 1))), res |> List.item 0)
    Assert.AreEqual(((0, -1), (3u, ('C', 3))), res |> List.item 1)
    Assert.AreEqual(((0, 1), (18u, ('R', 1))), res |> List.item 2)
    Assert.AreEqual(((0, 2), (5u, ('E', 1))), res |> List.item 3)

[<Test>]
let playWordDirectValidate () =
    let (sorted, pieces) = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    getHandId 'R' lookupTable
                    getHandId 'E' lookupTable ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let move =
        [ PlayedLetter((0, 0), (1u, ('C', 1)))
          PlayLetter((1, 0), (1u, ('A', 1)))
          PlayLetter((2, 0), (18u, ('R', 1)))
          PlayLetter((3, 0), (5u, ('E', 1))) ]

    Assert.IsTrue(validateMove state pieces move)

[<Test>]
let invalidMove1 () =
    let (_, pieces) = getSortedAndPieces ()
    let state = mockState []

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (1u, ('A', 1)))
                          ((0, 1), (2u, ('B', 1)))
                          ((0, 2), (12u, ('L', 1)))
                          ((0, 3), (5u, ('E', 1)))
                          ((1, 2), (9u, ('I', 1)))
                          ((2, 2), (15u, ('O', 1)))
                          ((3, 2), (14u, ('N', 1))) ])
            state

    let move =
        [ PlayLetter((1, -2), (3u, ('C', 1)))
          PlayLetter((1, -1), (1u, ('A', 1)))
          PlayLetter((1, 0), (18u, ('R', 1)))
          PlayLetter((1, 1), (5u, ('E', 1))) ]

    Assert.IsFalse(validateMove state pieces move)

[<Test>]
let invalidMove2 () =
    let (_, pieces) = getSortedAndPieces ()
    let state = mockState []

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (1u, ('A', 1)))
                          ((0, 1), (2u, ('B', 1)))
                          ((0, 2), (12u, ('L', 1)))
                          ((0, 3), (5u, ('E', 1))) ])
            state

    let move =
        [ PlayLetter((1, -2), (3u, ('C', 1)))
          PlayLetter((1, -1), (1u, ('A', 1)))
          PlayLetter((1, 0), (18u, ('R', 1)))
          PlayLetter((1, 1), (5u, ('E', 1))) ]

    Assert.IsFalse(validateMove state pieces move)

[<Test>]
let invalidMove3 () =
    let (_, pieces) = getSortedAndPieces ()
    let state = mockState []

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (1u, ('A', 1)))
                          ((0, 1), (2u, ('B', 1)))
                          ((0, 2), (12u, ('L', 1)))
                          ((0, 3), (5u, ('E', 1))) ])
            state

    let move =
        [ PlayLetter((0, -1), (3u, ('C', 1)))
          PlayLetter((1, -1), (1u, ('A', 1)))
          PlayLetter((2, -1), (18u, ('R', 1)))
          PlayLetter((3, -1), (5u, ('E', 1))) ]

    Assert.IsTrue(validateMove state pieces move)

[<Test>]
let invalidMove4 () =
    let (_, pieces) = getSortedAndPieces ()
    let state = mockState []

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, -1), (3u, ('C', 1)))
                          ((0, 0), (1u, ('A', 1)))
                          ((0, 1), (2u, ('B', 1)))
                          ((0, 2), (12u, ('L', 1)))
                          ((0, 3), (5u, ('E', 1))) ])
            state

    let move =
        [ PlayLetter((1, 1), (1u, ('A', 1)))
          PlayLetter((2, 1), (18u, ('R', 1)))
          PlayLetter((3, 1), (5u, ('E', 1))) ]

    // "BARE" doesn't exist in the dictionary
    Assert.IsFalse(validateMove state pieces move)

[<Test>]
let invalidMove5 () =
    let (_, pieces) = getSortedAndPieces ()
    let state = mockState []

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, -1), (3u, ('C', 1)))
                          ((0, 0), (1u, ('A', 1)))
                          ((0, 1), (2u, ('B', 1)))
                          ((0, 2), (12u, ('L', 1)))
                          ((0, 3), (5u, ('E', 1)))
                          ((1, 2), (9u, ('I', 1)))
                          ((2, 2), (15u, ('O', 1)))
                          ((3, 2), (14u, ('N', 1))) ])
            state

    let move =
        [ PlayLetter((1, 1), (1u, ('A', 1)))
          PlayLetter((2, 1), (18u, ('R', 1)))
          PlayLetter((3, 1), (5u, ('E', 1))) ]

    Assert.IsFalse(validateMove state pieces move)

[<Test>]
let playBlankWordDirect () =
    let (sorted, pieces) = getSortedAndPieces ()

    let state =
        mockState [ 0u
                    0u
                    0u ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal
    Assert.AreEqual(PlayedLetter((0, 0), (3u, ('C', 1))), res |> List.head |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (0u, ('A', 0))), res |> List.head |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (0u, ('R', 0))), res |> List.head |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (0u, ('E', 0))), res |> List.head |> List.item 3)

[<Test>]
let playBlankInMiddleWordDirect () =
    let (sorted, pieces) = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState [ getHandId 'A' lookupTable
                    0u
                    getHandId 'E' lookupTable ]

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal
    printf "%A" res

    Assert.AreEqual(PlayedLetter((0, 0), (3u, ('C', 1))), res |> List.head |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (1u, ('A', 1))), res |> List.head |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (0u, ('R', 0))), res |> List.head |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (5u, ('E', 1))), res |> List.head |> List.item 3)