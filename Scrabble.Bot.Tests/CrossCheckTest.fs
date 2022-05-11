module Scrabble.Bot.Tests.CrossCheckTests

open NUnit.Framework
open TestingUtil

let internal addConstraint coord contraint (st: State.state) =
    {st with crossCheck = Map.add coord contraint st.crossCheck }

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Set.ofSeq

[<TestCase(0, 0)>]
[<TestCase(1, 0)>]
[<TestCase(0, 1)>]
[<TestCase(-1, 0)>]
[<TestCase(0, -1)>]
[<TestCase(-1, -1)>]
[<TestCase(1, 1)>]
let getAllowedLetters_Empty_returnEverything (x, y) =
    
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

[<Test>]
let update_placeCoat_work () = 
    let empty = Set.empty

    let tTop = "AS" |> Set.ofSeq
    let tBot = "AIO" |> Set.ofSeq
    let aTop = "AZTJ" |> Set.ofSeq
    let aBot = "AHSNT" |> Set.ofSeq
    let oTop = "BDGHJLMNOPSTWY" |> Set.ofSeq
    let oBot = "DEF" |> Set.ofSeq
    let cBot = "H" |> Set.ofSeq

    let mockState = mockState [] [ "COAT";"CH";"BO";"DO";"GO";"HO";"JO";"LO";"MO";"NO";"OO";"PO";"SO";"TO";"WO";"YO";"OD";"OE";"OF";"AA";"ZA";"TA";"JA";"AH";"AS";"AN";"AT";"ST";"TA";"TO";"I"; "SCOAT"; "COATS"]
                                    |> CrossCheck.update [ 
                                                            ((0,0), (0u, ('C', 0)))
                                                            ((1,0), (0u, ('O', 0))) 
                                                            ((2,0), (0u, ('A', 0)))
                                                            ((3,0), (0u, ('T', 0)))]



    let resultCEnd     = CrossCheck.getAllowedLetters mockState (-1, 0)  
    let resultCTop     = CrossCheck.getAllowedLetters mockState (0, 1)   
    let resultCBot     = CrossCheck.getAllowedLetters mockState (0, -1)  
    let topLeftCorner  = CrossCheck.getAllowedLetters mockState (-1, 1)  
    let topRightCorner = CrossCheck.getAllowedLetters mockState (4, 1)   
    let botLeftCorner  = CrossCheck.getAllowedLetters mockState (-1, -1) 
    let botRightCorner = CrossCheck.getAllowedLetters mockState (4, -1)  
    let resultTopO     = CrossCheck.getAllowedLetters mockState (1, 1)   
    let resultBotO     = CrossCheck.getAllowedLetters mockState (1, -1)  
    let resultTopA     = CrossCheck.getAllowedLetters mockState (2, 1)   
    let resultBotA     = CrossCheck.getAllowedLetters mockState (2, -1)  
    let resultTopT     = CrossCheck.getAllowedLetters mockState (3, 1)   
    let resultBotT     = CrossCheck.getAllowedLetters mockState (3, -1)  
    let resultEndT     = CrossCheck.getAllowedLetters mockState (4, 0)   

    Assert.AreEqual(empty, resultCEnd)
    Assert.AreEqual(empty, resultEndT)
    Assert.AreEqual(alphabet, topLeftCorner)
    Assert.AreEqual(alphabet, topRightCorner)
    Assert.AreEqual(alphabet, botLeftCorner)
    Assert.AreEqual(alphabet, botRightCorner)
    Assert.AreEqual(empty, resultCTop)
    Assert.AreEqual(cBot, resultCBot)
    Assert.AreEqual(oTop, resultTopO)
    Assert.AreEqual(oBot, resultBotO)
    Assert.AreEqual(aTop, resultTopA)
    Assert.AreEqual(aBot, resultBotA)
    Assert.AreEqual(tTop, resultTopT)
    Assert.AreEqual(tBot, resultBotT)

[<Test>]
let update_placeCoatAndBest_work () = 
    let empty = Set.empty

    let tTop = "AS" |> Set.ofSeq
    let tBot = "AIO" |> Set.ofSeq
    let aTop = "AZTJ" |> Set.ofSeq
    let aBot = "AHSNT" |> Set.ofSeq
    let oTop = "BDGHJLMNOPSTWY" |> Set.ofSeq
    let oBot = "DEF" |> Set.ofSeq
    let cBot = "H" |> Set.ofSeq

    let mockState = mockState [] [ "COAT";"CH";"BO";"DO";"GO";"HO";"JO";"LO";"MO";"NO";"OO";"PO";"SO";"TO";"WO";"YO";"OD";"OE";"OF";"AA";"ZA";"TA";"JA";"AH";"AS";"AN";"AT";"ST";"TA";"TO";"I"; "SCOAT"; "COATS"]
                                    |> CrossCheck.update [ 
                                                            ((0,0), (0u, ('C', 0)))
                                                            ((1,0), (0u, ('O', 0))) 
                                                            ((2,0), (0u, ('A', 0)))
                                                            ((3,0), (0u, ('T', 0)))]
                                    |> CrossCheck.update [ 
                                                            ((1,1), (0u, ('B', 0)))
                                                            ((2,1), (0u, ('E', 0))) 
                                                            ((3,1), (0u, ('S', 0)))
                                                            ((4,1), (0u, ('T', 0)))]



    let resultCEnd     = CrossCheck.getAllowedLetters mockState (-1, 0)  
    let resultCTop     = CrossCheck.getAllowedLetters mockState (0, 1)   
    let resultCBot     = CrossCheck.getAllowedLetters mockState (0, -1)  
    let topLeftCorner  = CrossCheck.getAllowedLetters mockState (-1, 1)  
    let topRightCorner = CrossCheck.getAllowedLetters mockState (4, 1)   
    let botLeftCorner  = CrossCheck.getAllowedLetters mockState (-1, -1) 
    let botRightCorner = CrossCheck.getAllowedLetters mockState (4, -1)  
    let resultTopO     = CrossCheck.getAllowedLetters mockState (1, 1)   
    let resultBotO     = CrossCheck.getAllowedLetters mockState (1, -1)  
    let resultTopA     = CrossCheck.getAllowedLetters mockState (2, 1)   
    let resultBotA     = CrossCheck.getAllowedLetters mockState (2, -1)  
    let resultTopT     = CrossCheck.getAllowedLetters mockState (3, 1)   
    let resultBotT     = CrossCheck.getAllowedLetters mockState (3, -1)  
    let resultEndT     = CrossCheck.getAllowedLetters mockState (4, 0)   

    Assert.AreEqual(empty, resultCEnd)
    Assert.AreEqual(empty, resultEndT)
    Assert.AreEqual(alphabet, topLeftCorner)
    Assert.AreEqual(empty, topRightCorner)
    Assert.AreEqual(alphabet, botLeftCorner)
    Assert.AreEqual(alphabet, botRightCorner)
    Assert.AreEqual(empty, resultCTop)
    Assert.AreEqual(cBot, resultCBot)
    Assert.AreEqual(empty, resultTopO)
    Assert.AreEqual(empty, resultBotO)
    Assert.AreEqual(empty, resultTopA)
    Assert.AreEqual(empty, resultBotA)
    Assert.AreEqual(empty, resultTopT)
    Assert.AreEqual(empty, resultBotT)

