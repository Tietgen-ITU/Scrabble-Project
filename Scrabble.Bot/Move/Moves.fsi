module internal Moves

open State
open Play
open ScrabbleUtil

val getNextMove: state -> Map<uint32, tile> -> Option<List<coord * (uint32 * (char * int))>>
val gen: state -> Map<uint32, tile> -> coord -> Direction -> Play list
