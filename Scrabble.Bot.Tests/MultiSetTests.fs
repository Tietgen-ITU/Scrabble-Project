module Scrabble.Bot.Tests.MultiSetTests

open NUnit.Framework

open MultiSet

[<Test>]
let empty_Returns_Empty_MultiSet () =
    Assert.IsTrue(isEmpty empty)
    Assert.AreEqual(0u, size empty)

[<Test>]
let isEmpty_Returns_True_For_Empty_MultiSet () =
    let e = empty
    Assert.IsTrue(isEmpty e)

[<Test>]
let isEmpty_Returns_False_For_NonEmpty_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    Assert.IsFalse(isEmpty s)

[<Test>]
let size_Returns_Zero_For_Empty_MultiSet () =
    let e = empty
    Assert.AreEqual(0u, size e)

[<Test>]
let size_Returns_One_For_Single_Item_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    Assert.AreEqual(1u, size s)

[<Test>]
let size_Returns_Two_For_Two_Item_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "b" s
    Assert.AreEqual(2u, size s2)

[<Test>]
let size_Returns_Two_For_Two_Items_Of_Same_Value () =
    let mutable e = empty
    e <- addSingle "a" e
    e <- addSingle "a" e

    Assert.AreEqual(2u, size e)

[<Test>]
let contains_Returns_True_For_Single_Item_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    Assert.IsTrue(contains "a" s)

[<Test>]
let contains_Returns_False_For_Single_Item_MultiSet_With_Different_Value () =
    let e = empty
    let s = addSingle "a" e
    Assert.IsFalse(contains "b" s)

[<Test>]
let contains_Returns_True_For_Two_Item_MultiSet_With_Same_Value () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "a" s
    Assert.IsTrue(contains "a" s2)

[<Test>]
let contains_Returns_False_For_Two_Item_MultiSet_With_Different_Value () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "b" s
    Assert.IsFalse(contains "z" s2)

[<Test>]
let numItems_Returns_Zero_For_Empty_MultiSet () =
    let e = empty
    Assert.AreEqual(0u, numItems "a" e)

[<Test>]
let numItems_Returns_One_For_Single_Item_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    Assert.AreEqual(1u, numItems "a" s)

[<Test>]
let numItems_Returns_Zero_For_Single_Item_MultiSet_With_Different_Value () =
    let e = empty
    let s = addSingle "a" e
    Assert.AreEqual(0u, numItems "b" s)

[<Test>]
let numItems_Returns_Two_For_Two_Item_MultiSet_With_Same_Value () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "a" s
    Assert.AreEqual(2u, numItems "a" s2)

[<Test>]
let numItems_Returns_Zero_For_Two_Item_MultiSet_With_Different_Value () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "b" s
    Assert.AreEqual(0u, numItems "z" s2)

[<Test>]
let add_Adds_Single_Item_To_Empty_MultiSet () =
    let mutable e = empty
    e <- add "a" 1u e

    Assert.AreEqual(1u, size e)
    Assert.IsTrue(contains "a" e)

[<Test>]
let add_Adds_Single_Item_To_NonEmpty_MultiSet () =
    let mutable e = empty
    e <- add "a" 1u e
    e <- add "b" 1u e

    Assert.AreEqual(2u, size e)
    Assert.IsTrue(contains "a" e)
    Assert.IsTrue(contains "b" e)

[<Test>]
let add_Adds_Single_Item_With_2_Counts_To_NonEmpty_MultiSet () =
    let mutable e = empty
    e <- add "a" 2u e

    Assert.AreEqual(2u, size e)
    Assert.AreEqual(2u, numItems "a" e)

[<Test>]
let addSingle_Returns_New_MultiSet_With_Added_Item () =
    let e = empty
    let s = addSingle "a" e
    Assert.AreEqual(1u, size s)

[<Test>]
let addSingle_Returns_New_MultiSet_With_Added_Item_Of_Same_Value () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "a" s
    Assert.AreEqual(2u, size s2)

[<Test>]
let remove_Removes_Single_Item_From_NonEmpty_MultiSet () =
    let mutable e = empty
    e <- add "a" 1u e
    e <- add "b" 1u e
    e <- remove "a" 1u e

    Assert.AreEqual(1u, size e)
    Assert.IsTrue(contains "b" e)
    Assert.IsFalse(contains "a" e)

[<Test>]
let remove_Removes_Single_Item_From_NonEmpty_MultiSet_With_2_Counts () =
    let mutable e = empty
    e <- add "a" 2u e
    e <- remove "a" 1u e

    Assert.AreEqual(1u, size e)
    Assert.AreEqual(1u, numItems "a" e)

[<Test>]
let remove_Removes_Single_Item_From_NonEmpty_MultiSet_With_2_Counts_And_Removes_All () =
    let mutable e = empty
    e <- add "a" 2u e
    e <- remove "a" 2u e

    Assert.AreEqual(0u, size e)

[<Test>]
let removeSingle_Removes_Single_Item_From_NonEmpty_MultiSet () =
    let mutable e = empty
    e <- add "a" 1u e
    e <- add "b" 1u e
    e <- removeSingle "a" e

    Assert.AreEqual(1u, size e)
    Assert.IsTrue(contains "b" e)
    Assert.IsFalse(contains "a" e)

[<Test>]
let removeSingle_Removes_Single_Item_From_NonEmpty_MultiSet_With_2_Counts () =
    let mutable e = empty
    e <- add "a" 2u e
    e <- removeSingle "a" e

    Assert.AreEqual(1u, size e)
    Assert.AreEqual(1u, numItems "a" e)

[<Test>]
let removeSingle_Removes_Single_Item_From_NonEmpty_MultiSet_With_2_Counts_And_Removes_All () =
    let mutable e = empty
    e <- add "a" 2u e
    e <- removeSingle "a" e
    e <- removeSingle "a" e

    Assert.AreEqual(0u, size e)

[<Test>]
let fold_Folds_Over_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "b" s
    let s3 = addSingle "c" s2
    let s4 = addSingle "d" s3

    let result = fold (fun state _ value -> value + state) 0u s4

    Assert.AreEqual(4u, result)

[<Test>]
let foldBack_Folds_Over_MultiSet () =
    let e = empty
    let s = addSingle "a" e
    let s2 = addSingle "b" s
    let s3 = addSingle "c" s2
    let s4 = addSingle "d" s3

    let result = foldBack (fun _ value state -> state + value) s4 0u

    Assert.AreEqual(4u, result)

[<Test>]
let foldBack_Folds_Over_MultiSet_Key_Sum () =
    let e = empty
    let s = addSingle 2u e
    let s2 = addSingle 4u s
    let s3 = addSingle 1u s2
    let s4 = addSingle 2u s3

    let result = foldBack (fun key _ state -> state + key) s4 0u

    Assert.AreEqual(7u, result)

[<Test>]
let codejudge_bug_1 () =
    let res = (remove 7.0 8u (add 7.0 3u empty))

    Assert.AreEqual(0u, size res)
    Assert.AreEqual(0u, numItems 7.0 res)
    Assert.IsTrue(isEmpty res)

[<Test>]
let codejudge_bug_2 () =
    let res = (remove 7.0 8u (add 7.0 7u empty))

    Assert.AreEqual(0u, size res)
    Assert.AreEqual(0u, numItems 7.0 res)
    Assert.IsTrue(isEmpty res)

[<Test>]
let codejudge_bug_3 () =
    let res = removeSingle 7.0 (add 8.0 9u empty)

    Assert.AreEqual(9u, size res)
    Assert.AreEqual(0u, numItems 7.0 res)
    Assert.IsFalse(isEmpty res)

[<Test>]
let ofList_Creates_MultiSet_From_List () =
    let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
    let m = ofList l

    Assert.AreEqual(10u, size m)
    Assert.AreEqual(1u, numItems 1 m)
    Assert.AreEqual(1u, numItems 2 m)
    Assert.AreEqual(1u, numItems 3 m)
    Assert.AreEqual(1u, numItems 4 m)
    Assert.AreEqual(1u, numItems 5 m)
    Assert.AreEqual(1u, numItems 6 m)
    Assert.AreEqual(1u, numItems 7 m)
    Assert.AreEqual(1u, numItems 8 m)
    Assert.AreEqual(1u, numItems 9 m)
    Assert.AreEqual(1u, numItems 10 m)

[<Test>]
let ofList_Creates_MultiSet_From_List_With_Duplicates () =
    let l =
        [ 1
          2
          3
          4
          5
          6
          7
          8
          9
          10
          1
          2
          3
          4
          5
          6
          7
          8
          9
          10 ]

    let m = ofList l

    Assert.AreEqual(20u, size m)
    Assert.AreEqual(2u, numItems 1 m)
    Assert.AreEqual(2u, numItems 2 m)
    Assert.AreEqual(2u, numItems 3 m)
    Assert.AreEqual(2u, numItems 4 m)
    Assert.AreEqual(2u, numItems 5 m)
    Assert.AreEqual(2u, numItems 6 m)
    Assert.AreEqual(2u, numItems 7 m)
    Assert.AreEqual(2u, numItems 8 m)
    Assert.AreEqual(2u, numItems 9 m)
    Assert.AreEqual(2u, numItems 10 m)

[<Test>]
let toList_Creates_List_From_Map () =
    let mutable m = addSingle 1 empty
    m <- addSingle 2 m
    m <- addSingle 3 m
    m <- addSingle 4 m

    let l = toList m
    Assert.AreEqual(4, l.Length)
    Assert.IsTrue(List.contains 1 l)
    Assert.IsTrue(List.contains 2 l)
    Assert.IsTrue(List.contains 3 l)
    Assert.IsTrue(List.contains 4 l)

[<Test>]
let toList_Creates_List_From_Map_With_Duplicates () =
    let mutable m = add 1 2u empty
    m <- add 2 2u m
    m <- add 3 2u m
    m <- add 4 2u m

    let l = toList m
    Assert.AreEqual(8, l.Length)
    Assert.IsTrue(List.contains 1 l)
    Assert.IsTrue(List.contains 2 l)
    Assert.IsTrue(List.contains 3 l)
    Assert.IsTrue(List.contains 4 l)

[<Test>]
let map_Maps_Over_MultiSet () =
    let mutable m = addSingle 1 empty
    m <- addSingle 2 m
    m <- addSingle 3 m
    m <- addSingle 4 m

    let m2 = map (fun x -> x + 1) m

    Assert.AreEqual(4u, size m2)
    Assert.AreEqual(1u, numItems 2 m2)
    Assert.AreEqual(1u, numItems 3 m2)
    Assert.AreEqual(1u, numItems 4 m2)
    Assert.AreEqual(1u, numItems 5 m2)

[<Test>]
let untion_Unions_Two_MultiSets () =
    let mutable m = addSingle 1 empty
    m <- add 2 3u m
    m <- addSingle 3 m
    m <- addSingle 4 m
    m <- addSingle 7 m

    let mutable m2 = addSingle 5 empty
    m2 <- addSingle 6 m2
    m2 <- add 7 6u m2
    m2 <- addSingle 8 m2

    let m3 = union m m2

    Assert.AreEqual(15u, size m3)
    Assert.AreEqual(1u, numItems 1 m3)
    Assert.AreEqual(3u, numItems 2 m3)
    Assert.AreEqual(1u, numItems 3 m3)
    Assert.AreEqual(1u, numItems 4 m3)
    Assert.AreEqual(1u, numItems 5 m3)
    Assert.AreEqual(1u, numItems 6 m3)
    Assert.AreEqual(6u, numItems 7 m3)
    Assert.AreEqual(1u, numItems 8 m3)

[<Test>]
let subtract_Subtracts_Two_MultiSet () =
    let mutable m = addSingle 1 empty
    m <- addSingle 2 m
    m <- addSingle 3 m
    m <- addSingle 4 m

    let mutable m2 = addSingle 3 empty
    m2 <- add 4 3u m2
    m2 <- add 5 3u m2

    let m3 = subtract m m2

    Assert.AreEqual(2, size m3)
    Assert.AreEqual(1u, numItems 1 m3)
    Assert.AreEqual(1u, numItems 2 m3)
    Assert.AreEqual(0u, numItems 3 m3)
    Assert.AreEqual(0u, numItems 4 m3)
    Assert.AreEqual(0u, numItems 5 m3)

[<Test>]
let intersection_Intersects_Two_MultiSet () =
    let mutable m = addSingle 1 empty
    m <- addSingle 2 m
    m <- addSingle 3 m
    m <- addSingle 4 m

    let mutable m2 = addSingle 3 empty
    m2 <- add 4 3u m2
    m2 <- add 5 3u m2

    let m3 = intersection m m2

    Assert.AreEqual(2u, size m3)
    Assert.AreEqual(0u, numItems 1 m3)
    Assert.AreEqual(0u, numItems 2 m3)
    Assert.AreEqual(1u, numItems 3 m3)
    Assert.AreEqual(1u, numItems 4 m3)
    Assert.AreEqual(0u, numItems 5 m3)

[<Test>]
let codejudge_sum_1 () =
    let s31 =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i ]

    let s31' =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i
                         yield i ]

    Assert.AreEqual(15150u, (size (sum s31 s31')))

[<Test>]
let codejudge_sum_2 () =
    let s32 =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i ]

    let s32' =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i
                         yield i ]

    Assert.AreEqual(33u, numItems 90 (sum s32 s32'))

[<Test>]
let codejudge_sum_3 () =
    let s33 =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i ]

    let s33' =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i
                         yield i ]

    Assert.AreEqual(0u, (numItems 101 (sum s33 s33')))

[<Test>]
let codejudge_sum_4 () =
    let s34 =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i ]

    Assert.AreEqual(10100u, (size (sum s34 s34 |> map (fun i -> i % 10))))

[<Test>]
let codejudge_sum_5 () =
    let s35 =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i ]

    let s35' =
        ofList [ for i in 1..100 do
                     for _ in i..100 do
                         yield i
                         yield i ]

    Assert.AreEqual(1530u, (numItems 5 (sum s35 s35' |> map (fun i -> i % 10))))
