module internal Moves

open State
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open System.Text


type Direction =
    | Horizontal
    | Vertical

type Piece = (uint32 * (char * int))

type Move = ((coord * Piece))

type Play =
    | PlayLetter of Move
    | PlayedLetter of Move

type Plays = (Play list * Play list list)

type MessageRequest<'a> = 
    | SetValue of 'a
    | RequestValue of AsyncReplyChannel<'a>

let getNormalWord = 
    List.map (fun play -> match play with
                            | PlayLetter (c,(id,(l,p))) -> (c,(l,p))
                            | PlayedLetter (c,(id,(l,p))) -> (c,(l,p))
                            )

let getBestMove (state: State.state) (currentBest: (int * Play list)) (newPlay: Play list) =
    let newList = List.map (fun play -> match play with
                                                                            | PlayLetter (c,(id,(l,p))) -> (c,(l,p))
                                                                            | PlayedLetter (c,(id,(l,p))) -> (c,(l,p))
                                                        ) newPlay
    let newPoints = DIB.PointCalculator.calculateWordPoint newList state.board
    if newPoints > (fst currentBest) then (newPoints, newPlay) else currentBest

let createMoveMailbox st = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop word = 
            async {
                let! msg = inbox.Receive()
                match msg with
                | SetValue newWord -> return! loop (getBestMove st word newWord)
                | RequestValue replyCh -> 
                    replyCh.Reply (snd word)
                    return! loop word
            }
        

        loop (0, []))

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

let testCoord (st: state) (coord: coord) = hasLetter coord st.tilePlacement

let getLetter coordinate (state: State.state) =
    match state.tilePlacement.TryFind coordinate with
    | Some c -> Some c
    | None -> None

let nextArc (c: (uint32 * (char * int))) (arc: Dictionary.Dict) = Dictionary.step (c |> snd |> fst) arc

let getNextCoordinate (pos: coord) (offset: int32) (dir: Direction) =
    match dir with
    | Vertical -> (pos |> fst, (pos |> snd) + offset)
    | Horizontal -> ((pos |> fst) + offset, pos |> snd)

let getPlayMovesFromPlays (plays: Play list) : Move list =
    plays
    |> List.map (fun play ->
        match play with
        | PlayLetter move -> Some move
        | PlayedLetter _ -> None)
    |> List.filter Option.isSome
    |> List.map Option.get

let validateGetLetter (st: state) (coord: coord) (moves: Move list) : Option<Piece> =
    // Check if the coord is in moves
    let letterInMoves = List.tryFind (fun (c, _) -> coord.Equals(c)) moves

    // Check if there is a letter on the board on coord
    match getLetter coord st with
    | Some c -> // There is a letter on the board on coord
        match letterInMoves with // Check if the coord is in moves, if it is check the letter on the board and in the moves is the same
        | Some moveC -> if moveC.Equals(c) then Some c else None
        | None -> Some c // The coord is not in moves, so the letter on the board is valid
    | None -> // The coord is not on the board, so just return the move
        match letterInMoves with // Option.bind should be able to do this, but i refuses for some reason
        | Some (_, piece) -> Some piece
        | None -> None

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
        if offset <= 0 then
            offset - 1
        else
            offset + 1

    let rec aux (offset: int32) (coord: coord) (dict: Dictionary.Dict) (valid: bool) =
        match validateGetLetter st (getNextCoordinate coord offset direction) moves with
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
let validateMove (st: state) (pieces: Map<uint32, tile>) (plays: Play list) =
    // Move through each move
    // Test in each direction, go through like normal. So go up then down, then left, then right.
    // Each tested coord is added to the list of tested coords.
    // If the coord you are about to touch is already in the list of tested coords, then you have already tested it.

    let testCoordAt st direction coord letter moves =
        if testCoord st (getNextCoordinate coord 1 direction)
           || testCoord st (getNextCoordinate coord (-1) direction) then
            match nextArc letter st.dict with
            | Some (valid, newArc) -> validateDirection st direction coord newArc valid moves
            | None -> false
        else
            true

    let rec aux moves' moves =
        match moves' with
        | [] -> true
        | (coord, letter) :: rest ->
            match testCoordAt st Horizontal coord letter moves with
            | true ->
                match testCoordAt st Vertical coord letter moves with
                | true -> aux rest moves
                | false -> false
            | false -> false

    let moves = plays |> getPlayMovesFromPlays
    aux moves moves

let recordPlay
    (st: state)
    (pieces: Map<uint32, ScrabbleUtil.tile>)
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
     if isWord && validateMove st pieces played then
         played :: (plays |> snd)
     else
         plays |> snd)

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

let goOn
    genAux // The genAux function defined below
    (st: State.state)
    (pieces: Map<uint32, ScrabbleUtil.tile>) // TODO: I really don't like having to pass around the state and pieces if we don't need it in all the methods. Maybe we should move recordPlay to a lambda function, it would help with code duplication as well
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

    let roomToTheLeft =
        not ((hasLetter leftCoord st.tilePlacement))
        && hasSquare st.board leftCoord

    let roomToTheRight =
        not ((hasLetter rightCoord st.tilePlacement))
        && hasSquare st.board rightCoord

    if pos <= 0 then // Moving left
        let word = [ l ] @ word

        match newArc with
        | Some (isWord, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays =
                match roomToTheLeft with
                | true ->
                    recordPlay st pieces (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                    |> genAux st pieces anchor (pos - 1) direction word rack newArc
                | false -> plays

            // SwiExactly how these types, squares, and boards are used will be made clear in the inividual assignments.tch direction
            // let newArc = Dictionary.reverse newArc |> Option.get |> snd // TODO: This is ugly, and possibly unsafe

            match (Dictionary.reverse newArc) with
            | None -> plays
            | Some ((_, newArc)) ->
                if roomToTheLeft && roomToTheRight then
                    genAux st pieces anchor 1 direction word rack newArc plays
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
                recordPlay st pieces (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                |> genAux st pieces anchor (pos + 1) direction word rack newArc
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
    (pieces: Map<uint32, ScrabbleUtil.tile>)
    (anchor: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: (uint32 * (char * int)) list)
    (rack: (uint32 * (char * int)) list)
    (arc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn (genAux) state pieces anchor pos direction c word rack (nextArc c arc) arc true plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack
                (fun c rack' ->
                    goOn (genAux) state pieces anchor pos direction c word rack' (nextArc c arc) arc false plays)
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
    : Play list list =

    let pos = 0 // Should always be 0 when starting
    let word = List.Empty
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    genAux state pieces startPos pos dir word rack initArc ([], [])
    |> snd

let getNextMove (st: state) (pieces: Map<uint32, tile>) =

    let wordMailBox = createMoveMailbox st

    let createAsyncMoveCalculation coord dir =
        async {
            gen st pieces coord dir
            |> Seq.iter (fun item -> wordMailBox.Post (SetValue item))
        }

    let wordResult =
        if st.tilePlacement.IsEmpty then
            gen st pieces st.board.center Vertical
            |> List.filter (fun word -> not <| List.isEmpty word)
            |> List.map (fun x -> (DIB.PointCalculator.calculateWordPoint (getNormalWord x) st.board, x))
            |> List.sortByDescending (fun (points, _) -> points)
            |> List.item 0 |> snd
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
                
            let cts = new System.Threading.CancellationTokenSource()

            Async.Parallel (asyncCalculation, System.Environment.ProcessorCount)
            |> fun comp -> Async.RunSynchronously (comp, 1000, cts.Token) |> ignore
            
            // Return the best result of the computation
            wordMailBox.PostAndReply((fun x -> RequestValue x), 2000)

    if wordResult.Length = 0 then
        None
    else
        Some(getPlayMovesFromPlays wordResult)
