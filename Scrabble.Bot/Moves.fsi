module internal Moves

open State
open ScrabbleUtil

type Direction =
    | Horizontal
    | Vertical

val getNextMove : state -> Map<uint32, tile> -> Option<List<coord * (uint32 * (char * int))>>
val validateMove: state -> Map<uint32, tile> -> List<coord * (uint32 * (char * int))> -> bool
val gen : state -> Map<uint32, tile> -> coord -> Direction -> (coord * (uint32 * (char * int))) list list
