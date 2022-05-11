module MultiSet

open Microsoft.FSharp.Core.Operators.Checked

type MultiSet<'a when 'a: comparison> = Map<'a, uint32>

let getOrDefault (a: 'a) (s: MultiSet<'a>) : uint32 =
    match Map.tryFind a s with
    | Some x -> x
    | None -> 0u

// FIXME: This is cursed, there must be a better way
let (-) (a: uint32) (b: uint32) =
    try
        a - b
    with
    | :? System.OverflowException -> 0u

let max (a: uint32) (b: uint32) : uint32 = if a > b then a else b
let min (a: uint32) (b: uint32) : uint32 = if a < b then a else b

let empty: MultiSet<'a> = Map.empty<'a, uint32>

let fold (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) (s: MultiSet<'b>) : 'a = Map.fold f acc s

let foldBack (f: 'a -> uint32 -> 'b -> 'b) (s: MultiSet<'a>) (acc: 'b) : 'b = Map.foldBack f s acc

let size (s: MultiSet<'a>) : uint32 =
    s |> fold (fun state _ value -> state + value) 0u

let isEmpty (s: MultiSet<'a>) : bool = (size s).Equals(0u)

let contains (a: 'a) (s: MultiSet<'a>) : bool = getOrDefault a s > 0u

let numItems (a: 'a) (s: MultiSet<'a>) : uint32 = getOrDefault a s

let set (a: 'a) (n: uint32) (s: MultiSet<'a>) : MultiSet<'a> = s.Add(a, n)

let add (a: 'a) (n: uint32) (s: MultiSet<'a>) : MultiSet<'a> = set a (getOrDefault a s + n) s

let addSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = add a 1u s

let remove (a: 'a) (n: uint32) (s: MultiSet<'a>) : MultiSet<'a> =
    let s' = set a (getOrDefault a s - n) s

    match Map.tryFind a s' with
    | Some 0u -> s'.Remove a
    | _ -> s'

let removeSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = remove a 1u s

let ofList (l: 'a list) : MultiSet<'a> =
    l
    |> List.fold (fun state key -> addSingle key state) empty

let toList (s: MultiSet<'a>) : 'a list =
    s
    |> fold (fun state key value -> state @ [ for _ in 1u .. value -> key ]) []

let map (f: 'a -> 'b) (s: MultiSet<'a>) : MultiSet<'b> =
    s
    |> fold (fun state key value -> add (f key) value state) empty

let union (s1: MultiSet<'a>) (s2: MultiSet<'a>) : MultiSet<'a> =
    s1
    |> fold (fun state key value -> set key (max value (getOrDefault key s2)) state) s2

let sum (s1: MultiSet<'a>) (s2: MultiSet<'a>) : MultiSet<'a> =
    s1
    |> fold (fun state key value -> set key (value + getOrDefault key s2) state) s2

let subtract (s1: MultiSet<'a>) (s2: MultiSet<'a>) : MultiSet<'a> =
    s2
    |> fold (fun state key value -> remove key value state) s1

let intersection (s1: MultiSet<'a>) (s2: MultiSet<'a>) : MultiSet<'a> =
    // Uses the lowest common denominator
    // If there is 0 elements in one of the sets the it's set to 0 therefore empty
    // If there is more elements in one than the other, then the intersection is the lowest amount
    s1
    |> fold (fun state key value -> set key (min value (getOrDefault key s2)) state) empty
