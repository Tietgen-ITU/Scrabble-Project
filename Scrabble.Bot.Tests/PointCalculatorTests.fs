module Scrabble.Bot.Tests.PointCalculatorTest

open NUnit.Framework

open DIB.PointCalculator
open ScrabbleUtil

let internal b = Parser.mkBoard (StandardBoard.standardBoard ())

let letterH = ((2,3),('h',3))
let letterE = ((2,4),('e',1))
let letterY = ((2,5),('y',7))
let word = [letterH; letterE; letterY]


[<Test>]
let returns_correct_points () =
    let points = calculateWordPoint word b

    Assert.AreEqual(11, points)

