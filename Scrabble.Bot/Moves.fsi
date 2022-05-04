module internal Moves

open State
open ScrabbleUtil

type Direction =
    | Horizontal
    | Vertical

val getNextMove: state -> Map<uint32, tile> -> List<coord * char>
val validateMove: state -> Map<uint32, tile> -> List<coord * char> -> bool
