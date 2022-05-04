namespace DIB

open ScrabbleUtil


module internal PointCalculator =

    let private getSquare (coord: coord) (squares: Parser.boardFun2) = 
        squares coord 
        |> (fun (StateMonad.Success sq) -> sq) 
        |> Option.get

    let private getResult defvalue = function
        | StateMonad.Success a -> a
        | _ -> defvalue
    let calculateWordPoint (word : (coord * (char * int)) list) (board: Parser.board) : int = 
        let mapToList  (square:Map<int,Parser.squareFun>)  =
            Map.toList square 

        let mapToListFunc (word: (char*int) list) pos (square:Map<int,Parser.squareFun>) =
            Map.toList square |> List.map (fun (a, sqFun) -> (a, (fun acc -> sqFun word pos acc)))

        let letters = List.map (fun (_, letter) -> letter) word

        let accFunc acc (l, (c, i)) = (getSquare l board.squares) |> mapToList |> (@) acc 

        let appendedList = List.fold accFunc List.empty<int*Parser.squareFun> word
        let sortedList = List.sortByDescending (fun (k, _) -> k) appendedList
        0

        
    (*
        TODO:
        In order to calculate points we must do the following:
        - Create an anonymous function that takes in an accumulated result
        - Create a function(mapToListFunc) that takes the word and the position and creates the anonymous function
    *)

    
    

    