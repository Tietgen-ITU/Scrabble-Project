module Scrabble.Bot.Tests.CrossCheckTests

open NUnit.Framework
open TestingUtil

let internal addConstraint coord contraint (st: State.state) =
    {st with crossCheck = Map.add coord contraint st.crossCheck }

[<TestCase(0, 0)>]
[<TestCase(1, 0)>]
[<TestCase(0, 1)>]
[<TestCase(-1, 0)>]
[<TestCase(0, -1)>]
[<TestCase(-1, -1)>]
[<TestCase(1, 1)>]
let getAllowedLetters_Empty_returnEverything (x, y) =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Set.ofSeq
    let mockState = mockState List.empty List.Empty
                                    
    let result = CrossCheck.getAllowedLetters mockState (x, y)

    Assert.AreEqual(alphabet, result)

[<Test>]
let getAllowedLetters_hasARestrictiveMapOnCoord_returnEverything () =
    let a1 = "JKLMNOP" |> Set.ofSeq
    let a2 = "JKLMNOPAACFSDWQFFWRQW" |> Set.ofSeq
    let a3 = "POWHIWRIWNQRFJ0JKLMNOP" |> Set.ofSeq
    let mockState = mockState List.empty List.Empty
                                    |> addConstraint (0,0) a1
                                    |> addConstraint (0,1) a2
                                    |> addConstraint (1,1) a3
                                    |> addConstraint (-1, -1) a2


    let result00 = CrossCheck.getAllowedLetters mockState (0, 0)
    let result01 = CrossCheck.getAllowedLetters mockState (0, 1)
    let result02 = CrossCheck.getAllowedLetters mockState (1, 1)
    let result03 = CrossCheck.getAllowedLetters mockState (-1, -1)

    Assert.AreEqual(a1, result00)
    Assert.AreEqual(a2, result01)
    Assert.AreEqual(a3, result02)
    Assert.AreEqual(a2, result03)
