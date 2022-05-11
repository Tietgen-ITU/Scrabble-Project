module Scrabble.Bot.Tests.ValidationTests

open NUnit.Framework

open Scrabble.Bot.Tests.TestingUtil

open Play
open Validation

[<Test>]
let playWordDirectValidate () =
    let sorted, _ = getSortedAndPieces ()

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

    let move =
        [ PlayedLetter((0, 0), (1u, ('C', 1)))
          PlayLetter((1, 0), (1u, ('A', 1)))
          PlayLetter((2, 0), (18u, ('R', 1)))
          PlayLetter((3, 0), (5u, ('E', 1))) ]

    Assert.IsTrue(validateMove state move)

[<Test>]
let invalidMove1 () =
    let state = mockState [] defaultWords

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

    Assert.IsFalse(validateMove state move)

[<Test>]
let invalidMove2 () =
    let state = mockState [] defaultWords

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

    Assert.IsFalse(validateMove state move)

[<Test>]
let invalidMove3 () =
    let state = mockState [] defaultWords

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

    Assert.IsTrue(validateMove state move)

[<Test>]
let invalidMove4 () =
    let state = mockState [] defaultWords

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
    Assert.IsFalse(validateMove state move)

[<Test>]
let invalidMove5 () =
    let state = mockState [] defaultWords

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

    Assert.IsFalse(validateMove state move)
