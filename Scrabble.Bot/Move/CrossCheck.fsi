module internal CrossCheck

open State
open ScrabbleUtil

val getAllowedLetters: state -> coord -> Set<char>
val update: List<(coord * (uint32 * (char * int)))> -> state -> Map<coord, char>
