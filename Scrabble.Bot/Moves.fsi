module internal Moves

open State
open ScrabbleUtil

type Tile = uint32 * (char * int)

type Move = coord * Tile

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = Play list * Play list list

val getNextMove: state -> Map<uint32, tile> -> Option<List<coord * (uint32 * (char * int))>>
val validateMove: state -> List<Play> -> bool
val gen: state -> Map<uint32, tile> -> coord -> Direction -> Play list list
