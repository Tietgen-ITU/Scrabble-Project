namespace DIB

open ScrabbleUtil


module internal PointCalculator =

    let private getSquare (coord: coord) (squares: Parser.boardFun2) =
        squares coord
        |> (fun (StateMonad.Success sq) -> sq)
        |> Option.get

    let private getResult defvalue =
        function
        | StateMonad.Success a -> a
        | StateMonad.Failure _ -> defvalue

    let calculateWordPoint (word: (coord * (char * int)) list) (board: Parser.board) : int =
        let mapToListFunc (word: (char * int) list) pos (square: Map<int, Parser.squareFun>) =
            Map.toList square
            |> List.map (fun (a, sqFun) -> (a, (sqFun word pos)))

        let letters = List.map snd word

        (*
            What is done below:
                - Go through each char in the word(coord * (char * int)) with List.mapi and get squareFun to a list
                - Append the list above to one single list @
                - Sort the list above
                - Fold the list to get one int result which is the point scored for that word
        *)

        List.mapi (fun index (coord, _) -> mapToListFunc letters index (getSquare coord board.squares)) word
        |> List.fold (fun acc value -> value @ acc) List.Empty
        |> List.sortByDescending fst
        |> List.fold (fun acc (_, value) -> value acc |> getResult acc) 0
