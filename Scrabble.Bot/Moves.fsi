module internal Moves

open State
open ScrabbleUtil

type Direction =
    | Horizontal
    | Vertical

val getNextMove : state -> Map<uint32, tile> -> List<coord * char>