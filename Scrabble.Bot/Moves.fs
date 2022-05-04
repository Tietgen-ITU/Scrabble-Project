module internal Moves

open State
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open System.Text

type Direction =
    | Horizontal
    | Vertical

let listToString (chars: (uint32 * (char * int)) list) =
    string (List.fold (fun (sb: StringBuilder) c -> sb.Append(char (c |> snd |> fst))) (new StringBuilder()) chars)

let getRack (state: State.state) (pieces: Map<uint32, ScrabbleUtil.tile>) : (uint32 * (char * int)) list =
    state.hand
    |> MultiSet.fold
        (fun rack piece _ ->
            (Map.find piece pieces
             |> Set.toSeq
             |> Seq.head // FIXME: This doesn't handle blanks, which is good as that is supposed to be handled by the algorithm
             |> fun a -> (piece, (a)))
            :: rack)
        []

let recordPlay
    (pos: coord)
    (c: (uint32 * (char * int)))
    (word: (uint32 * (char * int)) list)
    (plays: (coord * (uint32 * (char * int))) list)
    : (coord * (uint32 * (char * int))) list =
    debugPrint (sprintf "Register play: (%d, %d) %A creating word: %s\n" (pos |> fst) (pos |> snd) c (listToString word))

    plays @ [ (pos, c) ]

let getLetter coordinate (state: State.state) =
    match state.tilePlacement.TryFind coordinate with
    | Some c -> Some c
    | None -> None

let getAllowedLetters (dict: Dictionary.Dict) =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ$" |> Seq.toList

    let rec aux (dict: Dictionary.Dict) (letters: char list) (allowed: Set<char>) =
        match letters with
        | [] -> allowed
        | letter :: letters ->
            match Dictionary.step letter dict with
            | Some _ -> aux dict letters (Set.add letter allowed)
            | None -> aux dict letters allowed

    aux dict alphabet Set.empty

let loopRack
    (f: (uint32 * (char * int)) -> (uint32 * (char * int)) list -> 'a list)
    (rack: (uint32 * (char * int)) list)
    (allowedLetters: Set<char>)
    : 'a list list =
    let rec aux (rack': (uint32 * (char * int)) list) (allowedLetters: Set<char>) (out: 'a list list) : 'a list list =
        match rack' with
        | [] -> out
        | tile :: rack' ->
            let out =
                if Set.contains (tile |> snd |> fst) allowedLetters then
                    match (f
                               tile
                               (rack
                                |> List.removeAt (rack |> List.findIndex (fun c -> c.Equals(tile)))))
                        with
                    | [] -> out
                    | plays -> plays :: out
                else
                    out

            aux rack' allowedLetters out

    aux rack allowedLetters []

let nextArc (c: (uint32 * (char * int))) (arc: Dictionary.Dict) = Dictionary.step (c |> snd |> fst) arc

let getNextCoordinate (pos: coord) (offset: int32) (dir: Direction) =
    match dir with
    | Horizontal -> (pos |> fst, (pos |> snd) + offset)
    | Vertical -> ((pos |> fst) + offset, pos |> snd)

let goOn
    genAux // The genAux function defined below
    (state: State.state)
    (anchor: coord)
    (pos: int32)
    (direction: Direction)
    (l: (uint32 * (char * int)))
    (word: (uint32 * (char * int)) list)
    (rack: (uint32 * (char * int)) list)
    (newArc: (bool * Dictionary.Dict) option)
    (oldArc: Dictionary.Dict)
    (plays: (coord * (uint32 * (char * int))) list)
    : (coord * (uint32 * (char * int))) list =
    let leftCoord = getNextCoordinate anchor (pos - 1) direction
    let rightCoord = getNextCoordinate anchor (pos + 1) direction

    let roomToTheLeft = not (hasLetter leftCoord state.tilePlacement)
    let roomToTheRight = not (hasLetter rightCoord state.tilePlacement)

    if pos <= 0 then // Moving left
        let word = [ l ] @ word

        match newArc with
        | Some (_, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays =
                match roomToTheLeft with
                | true ->
                    recordPlay (getNextCoordinate anchor pos direction) l word plays
                    |> genAux state anchor (pos - 1) direction word rack newArc
                | false -> []

            // SwiExactly how these types, squares, and boards are used will be made clear in the inividual assignments.tch direction
            // let newArc = Dictionary.reverse newArc |> Option.get |> snd // TODO: This is ugly, and possibly unsafe

            match (Dictionary.reverse newArc) with
            | None -> []
            | Some ((_, newArc)) ->
                if roomToTheLeft && roomToTheRight then
                    genAux state anchor 1 direction word rack newArc plays
                else
                    []
        | None ->
            debugPrint (sprintf "Not on old arc: offset: %d, (%d, %d) %c\n" pos (anchor |> fst) (anchor |> snd) (l |> snd |> fst)) // TODO: No clue if this ever happens, or how to handle it
            []
    else
        let word = word @ [ l ]

        match newArc with
        | Some (_, newArc) ->
            match roomToTheRight with
            | true ->
                recordPlay (getNextCoordinate anchor pos direction) l word plays
                |> genAux state anchor (pos + 1) direction word rack newArc
            | false -> []
        | None ->
            debugPrint (sprintf "Not on old arc: offset: %d, (%d, %d) %c\n" pos (anchor |> fst) (anchor |> snd) (l |> snd |> fst)) // TODO: No clue if this ever happens, or how to handle it
            []

let rec genAux
    (state: State.state)
    (anchor: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: (uint32 * (char * int)) list)
    (rack: (uint32 * (char * int)) list)
    (arc: Dictionary.Dict)
    (plays: (coord * (uint32 * (char * int))) list)
    : (coord * (uint32 * (char * int))) list =
    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn (genAux) state anchor pos direction c word rack (nextArc c arc) arc plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack
                (fun c rack' -> goOn (genAux) state anchor pos direction c word rack' (nextArc c arc) arc plays)
                rack
                allowedLetters

        match possiblePlays with
        | [] ->
            debugPrint (sprintf "Cannot play: %s\n" (listToString word))
            []
        | play :: _ -> play

// TODO: Handle blanks

let gen
    (state: State.state)
    (pieces: Map<uint32, ScrabbleUtil.tile>)
    (startPos: coord)
    (dir: Direction)
    : (coord * (uint32 * (char * int))) list =

    let pos = 0 // Should always be 0 when starting
    let word = List.Empty
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    let res = genAux state startPos pos dir word rack initArc []

    res

let getNextMove (st: state) (pieces: Map<uint32, tile>) =

    let createAsyncMoveCalculation coord dir =
        async {
            let result = gen st pieces coord dir
            return result
        }

    let asyncCalculation =
        st.tilePlacement
        |> Map.toList
        |> List.fold
            (fun acc (coord, _) ->
                [ (createAsyncMoveCalculation coord Horizontal) ]
                @ acc
                |> (@) [ (createAsyncMoveCalculation coord Vertical) ])
            List.Empty
        |> Async.Parallel

    let possibleWords =
        Async.RunSynchronously asyncCalculation
        |> Array.filter (fun word -> not <| List.isEmpty word)

    if possibleWords.Length = 0 then
        None
    else
        Some(possibleWords[0])

let testCoord (st: state) (coord: coord) = hasLetter coord st.tilePlacement

let validateDirection (st: state) (direction: Direction) (coord: coord) (dict: Dictionary.Dict) (valid: bool) =
    // Go backwards from coordinate
    // if you hit a blank spot, reverse direction
    // then go forwards from the coordinate
    // if you hit a blank spot, validate that it is a valid word

    let auxGetNextOffset (offset: int32) =
        if offset <= 0 then
            offset - 1
        else
            offset + 1

    let rec aux (offset: int32) (coord: coord) (dict: Dictionary.Dict) (valid: bool) =
        match getLetter (getNextCoordinate coord offset direction) st with
        | Some c ->
            match nextArc c dict with
            | Some (valid, newArc) -> aux (auxGetNextOffset offset) coord newArc valid
            | None ->
                debugPrint (sprintf "1. Can't go from %c at %A\n" (c |> snd |> fst) coord)
                false
        | _ ->
            if offset <= 0 then
                // Switch direction
                match Dictionary.reverse dict with
                | Some (valid, newArc) -> aux 1 coord newArc valid
                | None ->
                    debugPrint (sprintf "2. Can't reverse at %A\n" coord)
                    false // TODO: Not entirely sure this is right, might have to use the valid value
            else
                debugPrint (sprintf "Falling back to valid at %A\n" coord)
                valid
    // Start at -1 as the dictionary has already started at 0
    aux -1 coord dict valid

// Returns true if the move is valid, and false if it aint
let validateMove (st: state) (pieces: Map<uint32, tile>) (move: (coord * (uint32 * (char * int))) list) =
    // Move through each move
    // Test in each direction, go through like normal. So go up then down, then left, then right.
    // Each tested coord is added to the list of tested coords.
    // If the coord you are about to touch is already in the list of tested coords, then you have already tested it.

    let testAux st direction coord letter =
        if testCoord st (getNextCoordinate coord 1 direction)
           || testCoord st (getNextCoordinate coord (-1) direction) then
            match nextArc letter st.dict with
            | Some (valid, newArc) -> validateDirection st direction coord newArc valid
            | None -> false
        else
            true

    let rec aux move =
        match move with
        | [] -> true
        | (coord, letter) :: rest ->
            match testAux st Horizontal coord letter with
            | true ->
                match testAux st Vertical coord letter with
                | true -> aux rest
                | false -> false
            | false -> false

    aux move
