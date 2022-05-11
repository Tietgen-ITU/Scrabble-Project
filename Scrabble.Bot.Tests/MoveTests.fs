module Scrabble.Bot.Tests.MoveTests

open NUnit.Framework

open Scrabble.Bot.Tests.TestingUtil

open Moves
open Play

[<Test>]
let playWord () =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to suppress debug output
    let sorted, pieces = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState
            [ getHandId 'A' lookupTable
              getHandId 'R' lookupTable
              getHandId 'E' lookupTable ]
            defaultWords

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
    let sorted, pieces = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState
            [ getHandId 'A' lookupTable
              getHandId 'R' lookupTable
              getHandId 'E' lookupTable ]
            defaultWords

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal
    Assert.AreEqual(PlayedLetter((0, 0), (3u, ('C', 1))), res |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (1u, ('A', 1))), res |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (18u, ('R', 1))), res |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (5u, ('E', 1))), res |> List.item 3)

[<Test>]
let playFirstWord () =
    let sorted, pieces = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState
            [ getHandId 'A' lookupTable
              getHandId 'C' lookupTable
              getHandId 'R' lookupTable
              getHandId 'E' lookupTable ]
            defaultWords

    let state = State.placeLetters Seq.empty state

    let res = getNextMove state pieces |> Option.get

    Assert.AreEqual(((0, 0), (1u, ('A', 1))), res |> List.item 0)
    Assert.AreEqual(((0, -1), (3u, ('C', 3))), res |> List.item 1)
    Assert.AreEqual(((0, 1), (18u, ('R', 1))), res |> List.item 2)
    Assert.AreEqual(((0, 2), (5u, ('E', 1))), res |> List.item 3)

[<Test>]
let playBlankWordDirect () =
    let _, pieces = getSortedAndPieces ()

    let state = mockState [ 0u; 0u; 0u ] defaultWords

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal
    Assert.AreEqual(PlayedLetter((0, 0), (3u, ('C', 1))), res |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (0u, ('A', 0))), res |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (0u, ('R', 0))), res |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (0u, ('E', 0))), res |> List.item 3)

[<Test>]
let playBlankInMiddleWordDirect () =
    let sorted, pieces = getSortedAndPieces ()

    let lookupTable = getLookuptable sorted

    let state =
        mockState
            [ getHandId 'A' lookupTable
              0u
              getHandId 'E' lookupTable ]
            defaultWords

    let state =
        State.placeLetters
            (Seq.ofList [ ((0, 0), (3u, ('C', 1)))
                          ((0, 1), (1u, ('A', 1)))
                          ((0, 2), (2u, ('B', 1)))
                          ((0, 3), (12u, ('L', 1)))
                          ((0, 4), (5u, ('E', 1))) ])
            state

    let res = gen state pieces (0, 0) State.Horizontal

    Assert.AreEqual(PlayedLetter((0, 0), (3u, ('C', 1))), res |> List.item 0)
    Assert.AreEqual(PlayLetter((1, 0), (1u, ('A', 1))), res |> List.item 1)
    Assert.AreEqual(PlayLetter((2, 0), (0u, ('R', 0))), res |> List.item 2)
    Assert.AreEqual(PlayLetter((3, 0), (5u, ('E', 1))), res |> List.item 3)
