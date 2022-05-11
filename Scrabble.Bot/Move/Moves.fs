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
    (isWord: bool)
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =

    let played = (plays |> fst) @ [ makePlay pos c letterOnBoard ]

    debugPrint $"Register play: (%d{pos |> fst}, %d{pos |> snd}) %A{c} creating word: %s{playListToString played}\n"

    (played,
     match isWord && validateMove st played with
     | true -> played :: getWords plays
     | false -> getWords plays)

let loopPiece
    (goNext: uint32 * (char * int) -> Piece list -> Plays)
    restOfTheRack
    allowedLetters
    tile
    currentPlays
    : Plays =

    let letters =
        match tile with
        | Normal a -> [ a ]
        | Blank _ -> List.map (fun x -> (0u, (x, 0))) allowedLetters

    let rec goThrough lts plays =
        match lts with
        | [] -> plays
        | ch :: restOfLetters ->
            match (goNext ch restOfTheRack) with
            | _, [] -> goThrough restOfLetters plays
            | _, newPlays -> goThrough restOfLetters ([], newPlays @ (plays |> snd))

    goThrough letters currentPlays


let loopRack
    (f: uint32 * (char * int) -> Piece list -> Plays)
    (rack: Piece list)
    (allowedLetters: Set<char>)
    : Plays =
    let rec aux (rack': Piece list) (allowedLetters: Set<char>) (out: Plays) : Plays =
        match rack' with
        | [] -> out
        | tile :: rack' ->
            let out =
                match pieceIsAllowed allowedLetters tile with
                | true ->
                    loopPiece
                        f
                        (rack
                         |> List.removeAt (rack |> List.findIndex (fun c -> c.Equals(tile))))
                        (Seq.toList allowedLetters)
                        tile
                        out
                | false -> out

            aux rack' allowedLetters out

    aux rack allowedLetters ([], [])

let goOn
    genAux // The genAux function defined below
    (st: state)
    (anchor: coord)
    (pos: int32)
    (direction: Direction)
    (l: uint32 * (char * int))
    (rack: Piece list)
    (newArc: (bool * Dictionary.Dict) option)
    (letterOnBoard: bool)
    (plays: Plays)
    : Plays =
    let hasRoom off =
        let coord = getNextCoordinate anchor (pos + off) direction

        not (hasLetter coord st.tilePlacement)
        && hasSquare st.board coord

    let genAux pos newArc plays =
        genAux st anchor pos direction rack newArc plays

    let getPlays hasRoom newArc isWord offset =
        match hasRoom with
        | true ->
            recordPlay st (getNextCoordinate anchor pos direction) l isWord letterOnBoard plays
            |> genAux (pos + offset) newArc
        | false -> plays

    let roomToTheLeft = hasRoom -1
    let roomToTheRight = hasRoom 1
    let roomAround = roomToTheLeft && roomToTheRight

    match pos <= 0 with // Moving left
    | true ->
        match newArc with
        | Some (isWord, newArc) -> // We are on the old arc
            // Letter is on oldarc and there is no letter directly to the left
            let plays = getPlays roomToTheLeft newArc isWord -1

            switchDirection
                (fun _ newArc ->
                    match roomAround with
                    | true -> genAux 1 newArc plays
                    | false -> plays)
                plays
                newArc
        | None -> plays
    | false ->
        match newArc with
        | Some (isWord, newArc) -> getPlays roomToTheRight newArc isWord 1
        | None -> plays

let rec genAux
    (state: State.state)
    (anchor: coord)
    (pos: int32)
    (direction: Direction)
    (rack: Piece list)
    (arc: Dictionary.Dict)
    (plays: Plays)
    : Plays =
    let goOn c rack arc valid plays =
        goOn genAux state anchor pos direction c rack arc valid plays

    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some c -> goOn c rack (nextArc arc c) true plays
    | _ when rack.IsEmpty -> plays
    | None ->
        let allowedLetters = getAllowedLetters arc

        let possiblePlays =
            loopRack (fun c rack' -> goOn c rack' (nextArc arc c) false plays) rack allowedLetters

        match possiblePlays with
        | _, [] ->
            debugPrint $"Cannot play: %s{playsToString plays}\n"
            plays
        | plays -> plays

let gen (state: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (dir: Direction) : Play list =
    let pos = 0 // Should always be 0 when starting
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    let result =
        genAux state startPos pos dir rack initArc ([], [])
        |> snd
        |> List.map (fun x -> (DIB.PointCalculator.calculateWordPoint (getNormalWord x) state.board, x))
        |> List.sortByDescending fst

    match result with
    | [] -> List.Empty
    | _ -> List.item 0 result |> snd

let getNextMove (st: state) (pieces: Map<uint32, tile>) =
    let wordMailBox = createMoveMailbox st

    let createAsyncMoveCalculation coord dir =
        async {
            let result = gen st pieces coord dir
            wordMailBox.Post(SetValue result)
        }

    let wordResult =
        match st.tilePlacement with
        | x when x.IsEmpty -> gen st pieces st.board.center Vertical
        | _ ->
            let asyncCalculation =
                st.tilePlacement
                |> Map.toList
                |> List.fold
                    (fun acc (coord, _) ->
                        let moveCalcAcc dir acc =
                            match isBeginingOfWord coord dir st with
                            | true -> (createAsyncMoveCalculation coord dir) :: acc
                            | false -> acc

                        let horizontalMoveCalcAcc = moveCalcAcc Horizontal acc
                        moveCalcAcc Vertical horizontalMoveCalcAcc)
                    List.Empty

            let cts = new System.Threading.CancellationTokenSource()

            Async.Parallel(asyncCalculation, System.Environment.ProcessorCount)
            |> fun comp ->
                Async.RunSynchronously(comp, 1000, cts.Token)
                |> ignore

            // Return the best result of the computation
            wordMailBox.TryPostAndReply(RequestValue, 2000)
            |> Option.defaultValue []

    match wordResult with
    | [] -> None
    | _ -> Some(getPlayMovesFromPlays wordResult)
