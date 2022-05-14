module internal Play

open ScrabbleUtil

type Tile = uint32 * (char * int)

type Move = coord * Tile

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = Play list * Play list list

let makePlay pos c =
    function
    | true -> PlayedLetter(pos, c)
    | false -> PlayLetter(pos, c)

let getNormalTile =
    function
    | PlayLetter t -> t
    | PlayedLetter t -> t

let getNormalWord = List.map getNormalTile

let getPlayMovesFromPlays (plays: Play list) : Move list =
    plays
    |> List.map (fun play ->
        match play with
        | PlayLetter move -> Some move
        | PlayedLetter _ -> None)
    |> List.filter Option.isSome
    |> List.map Option.get

let getWords (plays: Plays) = plays |> snd
