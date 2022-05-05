module Scrabble.Bot.Tests.StateTests

open NUnit.Framework

open ScrabbleUtil

let internal b = Parser.mkBoard (StandardBoard.standardBoard ())

[<Test>]
let hasSquare_outSideBoundsOfNormalBoard_returnFalse () =
    
    let result = State.hasSquare b (10000, 100)
    Assert.IsFalse(result)

[<Test>]
let hasSquare_insideBoundsOfTheBoard_returnTrue () =
    
    let result = State.hasSquare b (0, 0)
    Assert.IsTrue(result)