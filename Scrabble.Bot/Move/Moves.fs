module internal Moves

open ScrabbleUtil
open ScrabbleUtil.DebugPrint

open State
open Piece
open Rack
open Play
open MoveUtil
open Validation
open Mailbox

let recordPlay
    (st: state)
    (pos: coord)
    (c: uint32 * (char * int))
    (word: (uint32 * (char * int)) list)
    (isWord: bool)
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =
    debugPrint $"Register play: (%d{pos |> fst}, %d{pos |> snd}) %A{c} creating word: %s{listToString word}\n"

    let played = (plays |> fst) @ [ makePlay pos c letterOnBoard ]

    (played,
     if isWord && validateMove st played then
         played :: (plays |> snd)
     else
         plays |> snd)

let loopRack (f: Piece -> Piece list -> Plays) (rack: Piece list) (allowedLetters: Set<char>) : Plays =
    let rec aux (rack': Piece list) (out: Plays) : Plays =
        match rack' with
        | [] -> out
        | tile :: rack' ->
            let out =
                if
                    not (isBlank tile)
                    && pieceIsAllowed allowedLetters tile
                then
                    (rack
                     |> List.removeAt (rack |> List.findIndex (fun c -> c.Equals(tile))))
                    |> f tile
                    |> function
                        | _, [] -> out
                        | _, plays -> ([], plays @ (out |> snd))
                else
                    out

            aux rack' out

    aux rack ([], [])

let loopBlank
    (f: Piece -> Piece list -> Plays)
    (blank: Piece)
    (restOfTheRack: Piece list)
    (allowedLetters: Set<char>)
    : Plays =
    let rec aux blankLetters rack out : Plays =
        match blankLetters with
        | [] -> out
        | tile :: blankLetters' ->
            let out =
                match (f tile rack) with
                | _, [] -> out
                | _, plays -> ([], plays @ (out |> snd))

            aux blankLetters' rack out

    aux (getAllowedPieces allowedLetters blank) restOfTheRack ([], [])

let goOn
    genAux // The genAux function defined below
    (st: state)
    (pieces: Map<uint32, tile>) // TODO: I really don't like having to pass around the state and pieces if we don't need it in all the methods. Maybe we should move recordPlay to a lambda function, it would help with code duplication as well
    (anchor: coord)
    (pos: int32)
    (direction: Direction)
    (l: uint32 * (char * int))
    (word: (uint32 * (char * int)) list)
    (rack: Piece list)
    (newArc: (bool * Dictionary.Dict) option)
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =
    let leftCoord = getNextCoordinate anchor (pos - 1) direction
    let rightCoord = getNextCoordinate anchor (pos + 1) direction

    let roomToTheLeft =
        not (hasLetter leftCoord st.tilePlacement)
        && hasSquare st.board leftCoord

    let roomToTheRight =
        not (hasLetter rightCoord st.tilePlacement)
        && hasSquare st.board rightCoord

    if pos <= 0 then // Moving left
        let word = [ l ] @ word

        match newArc with
        | Some (isWord, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays =
                match roomToTheLeft with
                | true ->
                    recordPlay st (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                    |> genAux st pieces anchor (pos - 1) direction word rack newArc
                | false -> plays

            // Switch direction
            match (Dictionary.reverse newArc) with
            | None -> plays
            | Some (_, newArc) ->
                if roomToTheLeft && roomToTheRight then
                    genAux st pieces anchor 1 direction word rack newArc plays
                else
                    plays
        | None ->
            debugPrint $"Not on old arc: offset: %d{pos}, (%d{anchor |> fst}, %d{anchor |> snd}) %c{l |> snd |> fst}\n" // TODO: No clue if this ever happens, or how to handle it

            plays
    else
        let word = word @ [ l ]

        match newArc with
        | Some (isWord, newArc) ->
            match roomToTheRight with
            | true ->
                recordPlay st (getNextCoordinate anchor pos direction) l word isWord letterOnBoard plays
                |> genAux st pieces anchor (pos + 1) direction word rack newArc
            | false -> plays
        | None ->
            debugPrint $"Not on old arc: offset: %d{pos}, (%d{anchor |> fst}, %d{anchor |> snd}) %c{l |> snd |> fst}\n" // TODO: No clue if this ever happens, or how to handle it

            plays

let rec genAux
    (state: State.state)
    (pieces: Map<uint32, tile>)
    (anchor: coord)
    (pos: int32)
    (direction: Direction)
    (word: (uint32 * (char * int)) list)
    (rack: Piece list)
    (arc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    let goOn c rack arc valid plays =
        goOn genAux state pieces anchor pos direction c word rack arc valid plays

    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn c rack (nextArc arc c) true plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack
                (fun c rack' ->
                    let normalPiece = getNormalPiece c

                    goOn normalPiece rack' (nextArc arc normalPiece) false plays)
                rack
                allowedLetters

        let possiblePlays =
            if hasBlank rack && (possiblePlays |> snd) = [] then
                let blank = List.find isBlank rack

                let remaining =
                    rack
                    |> List.removeAt (List.findIndex isBlank rack)

                let pl = (plays |> fst, possiblePlays |> snd)

                loopBlank
                    (fun c r ->
                        let normalPiece = getNormalPiece c

                        goOn normalPiece r (nextArc arc normalPiece) false pl)
                    blank
                    remaining
                    allowedLetters
            else
                possiblePlays

        match possiblePlays with
        | _, [] ->
            debugPrint $"Cannot play: %s{listToString word}\n"
            plays
        | plays -> plays

let gen (state: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (dir: Direction) : Play list list =
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
            |> Seq.iter (fun item -> wordMailBox.Post(SetValue item))
        }

    let wordResult =
        if st.tilePlacement.IsEmpty then
            gen st pieces st.board.center Vertical
            |> List.filter (fun word -> not <| List.isEmpty word)
            |> List.map (fun x -> (DIB.PointCalculator.calculateWordPoint (getNormalWord x) st.board, x))
            |> List.sortByDescending fst
            |> List.item 0
            |> snd
        else
            let asyncCalculation =
                st.tilePlacement
                |> Map.toList
                |> List.fold
                    (fun acc (coord, _) ->
                        let isBeginingOfWord direction = isBeginingOfWord coord direction st

                        let createAsyncMoveCalculation direction =
                            createAsyncMoveCalculation coord direction

                        let horizontalMoveCalcAcc =
                            if isBeginingOfWord Horizontal then
                                [ (createAsyncMoveCalculation Horizontal) ] @ acc
                            else
                                acc

                        if isBeginingOfWord Vertical then
                            (createAsyncMoveCalculation Vertical)
                            :: horizontalMoveCalcAcc
                        else
                            horizontalMoveCalcAcc)
                    List.Empty

            let cts = new System.Threading.CancellationTokenSource()

            Async.Parallel(asyncCalculation, System.Environment.ProcessorCount)
            |> fun comp ->
                Async.RunSynchronously(comp, 1000, cts.Token)
                |> ignore

            // Return the best result of the computation
            wordMailBox.PostAndReply(RequestValue, 2000)

    if wordResult.Length = 0 then
        None
    else
        Some(getPlayMovesFromPlays wordResult)
