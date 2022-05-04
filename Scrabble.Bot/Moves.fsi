module internal Moves

open State
open ScrabbleUtil

type Direction =
    | Horizontal
    | Vertical

val getNextMove : state -> Map<uint32, tile> -> Option<List<coord * char>>