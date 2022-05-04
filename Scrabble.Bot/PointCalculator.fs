namespace DIB

open ScrabbleUtil


module internal PointCalculator =

    let private getSquare (coord: coord) (squares: Parser.boardFun2) = 
        squares coord 
        |> (fun (StateMonad.Success sq) -> sq) 
        |> Option.get

    let private getResult defvalue = function
        | StateMonad.Success a -> a
        | StateMonad.Failure a -> printf "%A" a ; defvalue

    let calculateWordPoint (word : (coord * (char * int)) list) (board: Parser.board) : int = 

        let mapToList  (square:Map<int,Parser.squareFun>)  =
            Map.toList square 

        let mapToListFunc (word: (char*int) list) pos (square:Map<int,Parser.squareFun>) =
            Map.toList square |> List.map (fun (a, sqFun) -> (a, (fun acc -> sqFun word pos acc)))

        let letters = List.map (fun (_, letter) -> letter) word

         (*
            What is done below: 
                - Go through each char in the word(coord * (char * int)) with List.mapi and get squareFun to a list
                - Append the list above to one single list @
                - Sort the list above
                - Fold the list to get one int result which is the point scored for that word
        *)
        printf "%s\n" 
        List.iteri (fun index value -> printf "%A" value) word

        List.mapi(fun index (coord , _) -> mapToListFunc letters index (getSquare coord board.squares)) word
        |> List.fold (fun acc value -> value @ acc) List.Empty 
        |> List.sortByDescending (fun (priority , _) -> priority)  
        |> List.fold (fun acc (_,value) -> value acc |> getResult acc) 0
        
    
    

    