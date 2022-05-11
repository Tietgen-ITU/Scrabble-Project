module Moves

open State
open ScrabbleUtil.DebugPrint

type Direction =
    | Horizontal
    | Vertical

let getRack (state: State.state) (pieces: Map<uint32, 'a>) : 'a list =
    state.hand
    |> MultiSet.fold (fun rack piece _ -> (Map.find piece pieces) :: rack) []

let registerPlay (pos: ScrabbleUtil.coord) (c: char) =
    debugPrint (sprintf "Register play: (%d, %d) %c" (pos |> fst) (pos |> snd) c)

let gen (state: State.state) (pieces: Map<uint32, 'a>) =
    let rack = getRack state pieces

    0 // Replace

let genForReal
    (anchord: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: char list)
    (rack: char list)
    (arc: Dictionary.Dict)
    =
    failwith "Not implemented"
// Get letter on pos


let goOn
    (pos: int32)
    (c: char)
    (word: char list)
    (rack: char list)
    (newArc: Dictionary.Dict)
    (oldArd: Dictionary.Dict)
    =
    failwith "Not implemented"
