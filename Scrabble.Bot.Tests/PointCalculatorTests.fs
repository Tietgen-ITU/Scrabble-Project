module Scrabble.Bot.Tests.PointCalculatorTest

open NUnit.Framework

open DIB.PointCalculator
open ScrabbleUtil

let internal b = Parser.mkBoard (StandardBoard.standardBoard ())

let letterH = ((0, 0), (0u, ('h', 3)))
let letterE = ((0, 1), (0u, ('e', 1)))
let letterY = ((0, 2), (0u, ('y', 7)))
let word = [ letterH; letterE; letterY ]


[<Test>]
let returns_correct_points () =
    let points = calculateWordPoint word b

    Assert.AreEqual(11, points)
