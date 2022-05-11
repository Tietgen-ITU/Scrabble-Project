module internal Validation

open ScrabbleUtil
open ScrabbleUtil.DebugPrint

open State
open Play
open MoveUtil

let validateGetLetter (st: state) (coord: coord) (moves: Move list) : Option<Tile> =
    // Check if the coord is in moves
    let letterInMoves = List.tryFind (fun (c, _) -> coord.Equals(c)) moves

    // Check if there is a letter on the board on coord
    match getLetter coord st with
    | Some c -> // There is a letter on the board on coord
        match letterInMoves with // Check if the coord is in moves, if it is check the letter on the board and in the moves is the same
        | Some moveC -> if moveC.Equals(c) then Some c else None
        | None -> Some c // The coord is not in moves, so the letter on the board is valid
    | None -> // The coord is not on the board, so just return the move
        Option.bind (fun (_, piece) -> Some piece) letterInMoves

let validateDirection
    (st: state)
    (direction: Direction)
    (coord: coord)
    (dict: Dictionary.Dict)
    (valid: bool)
    (moves: Move list)
    =
    // Go backwards from coordinate
    // if you hit a blank spot, reverse direction
    // then go forwards from the coordinate
    // if you hit a blank spot, validate that it is a valid word

    let auxGetNextOffset (offset: int32) =
        match offset <= 0 with
        | true -> offset - 1
        | false -> offset + 1

    let getNextCoordinate coord offset =
        getNextCoordinate coord offset direction

    let rec aux (offset: int32) (coord: coord) (dict: Dictionary.Dict) (valid: bool) =
        match validateGetLetter st (getNextCoordinate coord offset) moves with
        | Some c ->
            match nextArc dict c with
            | Some (valid, newArc) -> aux (auxGetNextOffset offset) coord newArc valid
            | None -> false
        | _ ->
            match offset <= 0 with
            | true -> switchDirection (fun valid newArc -> aux 1 coord newArc valid) false dict
            | false -> valid
    // Start at -1 as the dictionary has already started at 0
    aux -1 coord dict valid

// Returns true if the move is valid, and false if it ain't
let validateMove (st: state) (plays: Play list) =
    // Move through each move
    // Test in each direction, go through like normal. So go up then down, then left, then right.
    // Each tested coord is added to the list of tested coords.
    // If the coord you are about to touch is already in the list of tested coords, then you have already tested it.

    let testCoordAt direction coord letter moves =
        let getNextCoordinate offset =
            getNextCoordinate coord offset direction

        match testCoord st (getNextCoordinate 1)
              || testCoord st (getNextCoordinate -1)
            with
        | true ->
            match nextArc st.dict letter with
            | Some (valid, newArc) -> validateDirection st direction coord newArc valid moves
            | None -> false
        | false -> true

    let rec aux moves' moves =
        match moves' with
        | [] -> true
        | (coord, letter) :: rest ->
            let testCoordAt direction =
                testCoordAt direction coord letter moves

            match testCoordAt Horizontal with
            | true ->
                match testCoordAt Vertical with
                | true -> aux rest moves
                | false -> false
            | false -> false

    let moves = plays |> getPlayMovesFromPlays
    aux moves moves
