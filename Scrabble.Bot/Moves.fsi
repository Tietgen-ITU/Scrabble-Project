module internal Moves

open State
open ScrabbleUtil

type Direction =
    | Horizontal
    | Vertical

type Move = ((coord * (uint32 * (char * int))))

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = (Play list * Play list list)

val getNextMove: state -> Map<uint32, tile> -> Option<List<coord * (uint32 * (char * int))>>
val validateMove: state -> Map<uint32, tile> -> List<coord * (uint32 * (char * int))> -> bool
val gen: state -> Map<uint32, tile> -> coord -> Direction -> Move list list
