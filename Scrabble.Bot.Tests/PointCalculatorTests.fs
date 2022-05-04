module Scrabble.Bot.Tests.PointCalculatorTest

open NUnit.Framework

open DIB.PointCalculator
open ScrabbleUtil

let internal b = Parser.mkBoard (StandardBoard.standardBoard ())

let letterH = ((0,0),('h',3))
let letterE = ((0,1),('e',1))
let letterY = ((0,2),('y',7))
let word = [letterH; letterE; letterY]


[<Test>]
let returns_correct_points () =
    let points = calculateWordPoint word b

    Assert.AreEqual(11, points)
