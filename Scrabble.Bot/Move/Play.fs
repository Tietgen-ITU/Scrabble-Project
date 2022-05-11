module internal Play

open ScrabbleUtil

type Tile = uint32 * (char * int)

type Move = coord * Tile

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = Play list * Play list list

let getNormalWord =
    List.map (fun play ->
        match play with
        | PlayLetter (c, (_, (l, p))) -> (c, (l, p))
        | PlayedLetter (c, (_, (l, p))) -> (c, (l, p)))

let getPlayMovesFromPlays (plays: Play list) : Move list =
    plays
    |> List.map (fun play ->
        match play with
        | PlayLetter move -> Some move
        | PlayedLetter _ -> None)
    |> List.filter Option.isSome
    |> List.map Option.get
