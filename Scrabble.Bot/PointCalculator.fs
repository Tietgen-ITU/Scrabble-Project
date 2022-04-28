namespace DIB

open ScrabbleUtil


module internal PointCalculator =

    let private getSquare (coord: coord) (squares: Parser.boardFun2) = 
        squares coord 
        |> (fun (StateMonad.Success sq) -> sq) 
        |> Option.get

    let calculateWordPoint (word : (coord * (char * int)) list) (board: Parser.board) : int = 
        let mapToList (square:Map<int,Parser.squareFun>) =
            Map.toList square 

        let appendedList = List.fold (fun acc (l, _) -> (getSquare l board.squares) |> mapToList |> (@) acc  ) List.empty<int*Parser.squareFun> word
        let sortedList = List.sortByDescending (fun (k, _) -> k) appendedList
        0


    
    

    