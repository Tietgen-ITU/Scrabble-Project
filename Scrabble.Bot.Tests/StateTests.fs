module Scrabble.Bot.Tests.StateTests

open NUnit.Framework

let internal b = Parser.mkBoard (ScrabbleUtil.StandardBoard.standardBoard ())

let internal mockState: State.state =
    let board = Parser.mkBoard (ScrabbleUtil.StandardBoard.standardBoard ())

    let dictAPI =
        Some(Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse)

    let dict = (ScrabbleUtil.Dictionary.mkDict Seq.empty dictAPI) true

    let playerId = 0u
    let hand = MultiSet.empty
    let players = []
    let playerTurn = 0u

    State.mkState board dict playerId hand players playerTurn

[<Test>]
let hasSquare_outSideBoundsOfNormalBoard_returnFalse () =

    let result = State.hasSquare b (10000, 100)
    Assert.IsFalse(result)

[<Test>]
let hasSquare_insideBoundsOfTheBoard_returnTrue () =

    let result = State.hasSquare b (0, 0)
    Assert.IsTrue(result)

[<Test>]
let isBeginingOfWord_atTheUpperLeftCornerVerticalAndHorizontal_returnTrue () =

    let st' =
        State.placeLetters
            [ ((-7, 7), (0u, ('A', 0)))
              ((-6, 7), (0u, ('A', 0)))
              ((-7, 6), (0u, ('A', 0))) ]
            mockState

    let horizontalResult = State.isBeginingOfWord (-7, 7) State.Horizontal st'
    let verticalResult = State.isBeginingOfWord (-7, 7) State.Vertical st'

    Assert.IsTrue(horizontalResult)
    Assert.IsTrue(verticalResult)

[<Test>]
let isBeginingOfWord_atTheUpperLeftCornerVerticalAndHorizontal_returnFalse () =

    let st' =
        State.placeLetters
            [ ((-7, 7), (0u, ('A', 0)))
              ((-6, 7), (0u, ('A', 0)))
              ((-7, 6), (0u, ('A', 0))) ]
            mockState

    let horizontalResult = State.isBeginingOfWord (-6, 7) State.Horizontal st'
    let verticalResult = State.isBeginingOfWord (-7, 6) State.Vertical st'

    Assert.IsFalse(horizontalResult)
    Assert.IsFalse(verticalResult)
