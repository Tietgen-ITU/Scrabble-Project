module internal CrossCheck

open State
open Play
open ScrabbleUtil

val getAllowedLetters: state -> coord -> Set<char>
val update: List<Move> -> state -> state
