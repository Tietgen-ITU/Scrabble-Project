module internal Moves

open State
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open System.Text

type Direction =
    | Horizontal
    | Vertical

type Move = ((coord * (uint32 * (char * int))))

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = (Play list * Play list list)

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
    (isWord: bool)
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =
    debugPrint (
        sprintf "Register play: (%d, %d) %A creating word: %s\n" (pos |> fst) (pos |> snd) c (listToString word)
    )

    let play =
        match letterOnBoard with
        | true -> PlayedLetter(pos, c)
        | false -> PlayLetter(pos, c)

    let played = (plays |> fst) @ [ play ]

    (played,
     if isWord then
         played :: (plays |> snd)
     else
         plays |> snd)

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
    (f: (uint32 * (char * int)) -> (uint32 * (char * int)) list -> Plays)
    (rack: (uint32 * (char * int)) list)
    (allowedLetters: Set<char>)
    : Plays =
    let rec aux (rack': (uint32 * (char * int)) list) (allowedLetters: Set<char>) (out: Plays) : Plays =
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
                    | (_, []) -> out
                    | (_, plays) -> ([], plays @ (out |> snd))
                else
                    out

            aux rack' allowedLetters out

    aux rack allowedLetters ([], [])

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
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =
    let leftCoord = getNextCoordinate anchor (pos - 1) direction
    let rightCoord = getNextCoordinate anchor (pos + 1) direction

    let roomToTheLeft = not ((hasLetter leftCoord state.tilePlacement) && hasSquare state.board leftCoord)
    let roomToTheRight = not ((hasLetter rightCoord state.tilePlacement) && hasSquare state.board rightCoord)

    if pos <= 0 then // Moving left
        let word = [ l ] @ word

        match newArc with
        | Some (isWord, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays =
                match roomToTheLeft with
                | true ->
                    recordPlay (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                    |> genAux state anchor (pos - 1) direction word rack newArc
                | false -> plays

            // SwiExactly how these types, squares, and boards are used will be made clear in the inividual assignments.tch direction
            // let newArc = Dictionary.reverse newArc |> Option.get |> snd // TODO: This is ugly, and possibly unsafe

            match (Dictionary.reverse newArc) with
            | None -> plays
            | Some ((_, newArc)) ->
                if roomToTheLeft && roomToTheRight then
                    genAux state anchor 1 direction word rack newArc plays
                else
                    plays
        | None ->
            debugPrint (
                sprintf
                    "Not on old arc: offset: %d, (%d, %d) %c\n"
                    pos
                    (anchor |> fst)
                    (anchor |> snd)
                    (l |> snd |> fst)
            ) // TODO: No clue if this ever happens, or how to handle it

            plays
    else
        let word = word @ [ l ]

        match newArc with
        | Some (isWord, newArc) ->
            match roomToTheRight with
            | true ->
                recordPlay (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                |> genAux state anchor (pos + 1) direction word rack newArc
            | false -> plays
        | None ->
            debugPrint (
                sprintf
                    "Not on old arc: offset: %d, (%d, %d) %c\n"
                    pos
                    (anchor |> fst)
                    (anchor |> snd)
                    (l |> snd |> fst)
            ) // TODO: No clue if this ever happens, or how to handle it

            plays

let rec genAux
    (state: State.state)
    (anchor: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: (uint32 * (char * int)) list)
    (rack: (uint32 * (char * int)) list)
    (arc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn (genAux) state anchor pos direction c word rack (nextArc c arc) arc true plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack
                (fun c rack' -> goOn (genAux) state anchor pos direction c word rack' (nextArc c arc) arc false plays)
                rack
                allowedLetters

        match possiblePlays with
        | (_, []) ->
            debugPrint (sprintf "Cannot play: %s\n" (listToString word))
            plays
        | plays -> plays

// TODO: Handle blanks

let gen
    (state: State.state)
    (pieces: Map<uint32, ScrabbleUtil.tile>)
    (startPos: coord)
    (dir: Direction)
    : Move list list =

    let pos = 0 // Should always be 0 when starting
    let word = List.Empty
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    genAux state startPos pos dir word rack initArc ([], [])
    |> snd
    |> List.fold
        (fun state t ->
            ((List.map
                (fun y ->
                    match y with
                    | PlayLetter x -> Some x
                    | PlayedLetter _ -> None)
                t)
             |> List.filter Option.isSome
             |> List.map Option.get)
            :: state)
        []

let getNextMove (st: state) (pieces: Map<uint32, tile>) =

    let createAsyncMoveCalculation coord dir =
        async {
            let result = gen st pieces coord dir
            return result
        }

    let possibleWords =
        if st.tilePlacement.IsEmpty then
            gen st pieces st.board.center Vertical
            |> List.filter (fun word -> not <| List.isEmpty word)
            |> List.sortByDescending (fun word -> word |> List.length)
        else
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

            Async.RunSynchronously asyncCalculation
            |> Array.fold (fun acc words -> acc @ words) List.Empty
            |> List.filter (fun word -> not <| List.isEmpty word)
            |> List.sortByDescending (fun word -> word |> List.length)

    // TODO: This might slow us down
    List.iter (fun x -> debugPrint (sprintf "Word: %s\n" (List.map snd x |> listToString))) possibleWords

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
