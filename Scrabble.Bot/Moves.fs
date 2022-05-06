module internal Moves

open State
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open System.Text

type Direction =
    | Horizontal
    | Vertical

type Piece = 
    | Normal of (uint32 * (char * int))
    | Blank of (uint32 * Map<char, int>) 

type Plays = ((coord * (uint32 * (char * int))) list * (coord * (uint32 * (char * int))) list list)

let isBlank = function
    | Normal _ -> false
    | Blank _ -> true

let hasBlank = 
    List.exists (fun p -> isBlank p)

let getNormalPiece = function
    | Normal a -> a
    | _ -> failwith "this is not a normal piece"

let getPieceAsBlank = function
    | Normal _ -> failwith "this is not a blank piece"
    | Blank a -> a

let pieceIsAllowed allowedLetters = function
    | Normal (_, (ch, _)) -> Set.contains ch allowedLetters
    | Blank _ -> true

(*
    Provides with a list of normal pieces. 
    If the Piece is of type Blank then the blank will return a list of normal pieces with all the letters in the allowedLetters
*)
let getAllowedPieces allowedPieces = function
    | Normal a -> [Normal a]
    | Blank (id, _) -> Set.fold (fun acc piece -> Normal (id, (piece, 0)) :: acc) List.Empty allowedPieces

let listToString (chars: (uint32 * (char * int)) list) =
    string (List.fold (fun (sb: StringBuilder) c -> sb.Append(char (c |> snd |> fst))) (new StringBuilder()) chars)

let returnPlays (plays: Plays) : Plays = ([], plays |> snd)

let getRack (state: State.state) (pieces: Map<uint32, ScrabbleUtil.tile>) : Piece list =
    let convertToPiece pieceId a =
        match pieceId with
        | 0u -> Blank (pieceId, Map.ofSeq a)
        | _ -> Normal (pieceId, Seq.head a)

    let rec createPieces rack piece pieces = function
        | 0u -> rack
        | x -> createPieces ((Map.find piece pieces |> Set.toSeq |> convertToPiece piece ) :: rack) piece pieces (x-1u)

    state.hand
    |> MultiSet.fold
        (fun rack piece amount -> createPieces rack piece pieces amount)
        []

let recordPlay
    (pos: coord)
    (c: (uint32 * (char * int)))
    (word: (uint32 * (char * int)) list)
    (isWord: bool)
    (plays: Plays)
    : Plays =
    debugPrint (
        sprintf "Register play: (%d, %d) %A creating word: %s\n" (pos |> fst) (pos |> snd) c (listToString word)
    )

    let played = (plays |> fst) @ [ (pos, c) ]

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
    (f: Piece -> Piece list -> Plays)
    (rack: Piece list)
    (allowedLetters: Set<char>)
    : Plays =
    let rec aux (rack': Piece list) (allowedLetters: Set<char>) (out: Plays) : Plays =
        match rack' with
        | [] -> out
        | tile :: rack' ->
            let out =
                if not(isBlank tile) && pieceIsAllowed allowedLetters tile then
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

let loopBlank
    (f: Piece -> Piece list -> Plays)
    (blank: Piece)
    (restOfTheRack: Piece list)
    (allowedLetters: Set<char>) 
    : Plays =
    let rec aux r rack out : Plays =
        match r with 
        | [] -> out
        | tile :: blank' -> 
            let out = 
                match (f tile rack) with
                | (_, []) -> out
                | (_, plays) -> ([], plays @ (out |> snd))

            aux blank' rack out

    aux (getAllowedPieces allowedLetters blank) restOfTheRack ([], [])

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
    (rack: Piece list)
    (newArc: (bool * Dictionary.Dict) option)
    (oldArc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    let leftCoord = getNextCoordinate anchor (pos - 1) direction
    let rightCoord = getNextCoordinate anchor (pos + 1) direction

    let roomToTheLeft = not (hasLetter leftCoord state.tilePlacement)
    let roomToTheRight = not (hasLetter rightCoord state.tilePlacement)

    if pos <= 0 then // Moving left
        let word = [ l ] @ word

        match newArc with
        | Some (isWord, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays =
                match roomToTheLeft with
                | true ->
                    recordPlay (getNextCoordinate anchor pos direction) l word isWord plays
                    |> genAux state anchor (pos - 1) direction word rack newArc
                | false -> returnPlays plays

            // SwiExactly how these types, squares, and boards are used will be made clear in the inividual assignments.tch direction
            // let newArc = Dictionary.reverse newArc |> Option.get |> snd // TODO: This is ugly, and possibly unsafe

            match (Dictionary.reverse newArc) with
            | None -> plays
            | Some ((_, newArc)) ->
                if roomToTheLeft && roomToTheRight then
                    genAux state anchor 1 direction word rack newArc plays
                else
                    returnPlays plays
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
                recordPlay (getNextCoordinate anchor pos direction) l word isWord plays
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

            returnPlays plays

let rec genAux
    (state: State.state)
    (anchor: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: (uint32 * (char * int)) list)
    (rack: Piece list)
    (arc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn (genAux) state anchor pos direction c word rack (nextArc c arc) arc plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack
                (fun c rack' -> goOn (genAux) state anchor pos direction (getNormalPiece c) word rack' (nextArc (getNormalPiece c) arc) arc plays)
                (rack |> List.filter (fun a -> not(isBlank a)))
                allowedLetters

        let possiblePlays =
            if hasBlank rack then
                let blank = List.find isBlank rack
                let remaining = List.removeAt (List.findIndex isBlank rack) rack 
                let pl = (plays |> fst, possiblePlays |> snd)

                loopBlank 
                    (fun c r -> goOn (genAux) state anchor pos direction (getNormalPiece c) word r (nextArc (getNormalPiece c) arc) arc pl)
                    blank
                    remaining
                    allowedLetters
            else possiblePlays
        
        match possiblePlays with
        | (_, []) ->
            debugPrint (sprintf "Cannot play: %s\n" (listToString word))
            returnPlays plays
        | plays -> returnPlays plays

let gen
    (state: State.state)
    (pieces: Map<uint32, ScrabbleUtil.tile>)
    (startPos: coord)
    (dir: Direction)
    : (coord * (uint32 * (char * int))) list list =

    let pos = 0 // Should always be 0 when starting
    let word = List.Empty
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    genAux state startPos pos dir word rack initArc ([], [])
    |> snd

let getNextMove (st: state) (pieces: Map<uint32, tile>) =

    let createAsyncMoveCalculation coord dir =
        async {
            let result = gen st pieces coord dir
            return result
        }

    let possibleWords =
        if st.tilePlacement.IsEmpty then
            gen st pieces (0, 0) Horizontal
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
