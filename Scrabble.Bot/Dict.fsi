module Dictionary

open System.Collections.Generic

type Dict = D of char * Dictionary<char, Dict> * bool

val empty : unit -> Dict
val insert : string -> Dict -> Dict
val step : char -> Dict -> (bool * Dict) option
val reverse : Dict -> (bool * Dict) option
val lookup : string -> Dict -> bool
