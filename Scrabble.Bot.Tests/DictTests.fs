module Scrabble.Bot.Tests.DictTests

open NUnit.Framework

open Dictionary

[<Test>]
let lookup_Lookup_Care () =
    let dict = insert "care" (empty ())
    let word = "care"

    let result = lookup word dict
    Assert.IsTrue(result)

[<Test>]
let lookup_Lookup_Cares () =
    let dict = insert "cares" (empty ())
    let word = "cares"

    let result = lookup word dict
    Assert.IsTrue(result)

[<Test>]
let lookup_Lookup_Partial_Word () =
    let dict = insert "cares" (empty ())
    let word = "care"

    let result = lookup word dict
    Assert.IsFalse(result)

[<Test>]
let lookup_Lookup_Not_Existing_Word () =
    let dict = insert "cares" (empty ())
    let word = "carez"

    let result = lookup word dict
    Assert.IsFalse(result)

[<Test>]
let insert_lookup_Many_Words () =
    let mutable dict = empty ()
    dict <- insert "care" dict
    dict <- insert "cares" dict
    dict <- insert "carez" dict
    dict <- insert "hello" dict
    dict <- insert "give me strength" dict
    dict <- insert "give me" dict

    Assert.IsTrue(lookup "care" dict)
    Assert.IsFalse(lookup "car" dict) // Partials are not accepted
    Assert.IsFalse(lookup "give" dict)
    Assert.IsTrue(lookup "hello" dict)
    Assert.IsFalse(lookup "hell" dict)
    Assert.IsTrue(lookup "give me" dict)

    Assert.IsFalse(lookup "caress" dict)
    Assert.IsFalse(lookup "cary" dict)

[<Test>]
let codejudge_bug_1() =
   let res = lookup "HE" (empty () |> insert "HELLO")
   
   Assert.IsFalse(res)

[<Test>]
let green_4_02_Test07_01 () =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let fromFile (path: string) =
        readLines path
        |> Seq.fold (fun acc s -> insert s acc) (empty ())

    let dict =
        fromFile "../../../EnglishDictionary.txt"

    Assert.IsTrue(lookup "TONIGHT" dict)

[<Test>]
let yellow_4_02_Test02_1 () =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let ss =
        readLines "../../../EnglishDictionary.txt"
        |> Seq.toList

    // Insert all
    let dict =
        List.fold (fun acc s -> insert s acc) (empty ()) ss

    // Reverse a string
    let reverse (s: string) =
        Seq.rev s |> Seq.toArray |> System.String

    Assert.IsTrue(lookup (reverse "AA") dict)
    Assert.IsFalse(lookup (reverse "AAH") dict)
    Assert.IsFalse(lookup (reverse "AAL") dict)
    Assert.IsTrue(lookup (reverse "AB") dict)
    Assert.IsTrue(lookup (reverse "ABA") dict)
    Assert.IsTrue(lookup (reverse "ABBA") dict)
    Assert.IsTrue(lookup (reverse "ABO") dict)
