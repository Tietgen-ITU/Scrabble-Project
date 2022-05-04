module internal Moves

open State
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open System.Text

type Direction =
    | Horizontal
    | Vertical

let listToString (chars: char list) =
    string (List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append(c)) (new StringBuilder()) chars)

let getRack (state: State.state) (pieces: Map<uint32, ScrabbleUtil.tile>) : char list =
    state.hand
    |> MultiSet.fold
        (fun rack piece _ ->
            (Map.find piece pieces
             |> Set.toSeq
             |> Seq.head // FIXME: This doesn't handle blanks, which is good as that is supposed to be handled by the algorithm
             |> fst)
            :: rack)
        []

let recordPlay (pos: coord) (c: char) (word: char list) (plays: (coord * char) list) : (coord * char) list =
    printf "Register play: (%d, %d) %c creating word: %s\n" (pos |> fst) (pos |> snd) c (listToString word)

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

let loopRack (f: char -> char list -> 'a list) (rack: char list) (allowedLetters: Set<char>) : 'a list list =
    let rec aux (rack': char list) (allowedLetters: Set<char>) (out: 'a list list) : 'a list list =
        match rack' with
        | [] -> out
        | letter :: rack' ->
            let out =
                if Set.contains letter allowedLetters then
                    match (f
                               letter
                               (rack
                                |> List.removeAt (rack |> List.findIndex (fun c -> c.Equals(letter)))))
                        with
                    | [] -> out
                    | plays -> plays :: out
                else
                    out

            aux rack' allowedLetters out

    aux rack allowedLetters []

let nextArc (c: char) (arc: Dictionary.Dict) = Dictionary.step c arc

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
    (l: char)
    (word: char list)
    (rack: char list)
    (newArc: (bool * Dictionary.Dict) option)
    (oldArc: Dictionary.Dict)
    (plays: (coord * char) list)
    : (coord * char) list =
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
            printf "%A - %A - %A - %A\n" anchor newArc word rack
            // let newArc = Dictionary.reverse newArc |> Option.get |> snd // TODO: This is ugly, and possibly unsafe

            let tempArc = Dictionary.reverse newArc

            if tempArc = None then []
            else 
                let newArc = tempArc |> Option.get |> snd
                if roomToTheLeft && roomToTheRight then
                    genAux state anchor 1 direction word rack newArc plays
                else
                    []
        | None ->
            printf "Not on old arc: offset: %d, (%d, %d) %c\n" pos (anchor |> fst) (anchor |> snd) l // TODO: No clue if this ever happens, or how to handle it
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
            printf "Not on old arc: offset: %d, (%d, %d) %c\n" pos (anchor |> fst) (anchor |> snd) l // TODO: No clue if this ever happens, or how to handle it
            []

let rec genAux
    (state: State.state)
    (anchor: ScrabbleUtil.coord)
    (pos: int32)
    (direction: Direction)
    (word: char list)
    (rack: char list)
    (arc: Dictionary.Dict)
    (plays: (coord * char) list)
    : (coord * char) list =
    match getLetter (getNextCoordinate anchor pos direction) state with
    | Some (c, _) -> goOn (genAux) state anchor pos direction c word rack (nextArc c arc) arc plays
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
            printf "Cannot play: %s\n" (listToString word)
            []
        | play :: _ -> play

// TODO: Handle blanks

let gen (state: State.state) (pieces: Map<uint32, ScrabbleUtil.tile>) (startPos: coord) (dir: Direction) : (coord * char) list =
    
    let pos = 0 // Should always be 0 when starting
    let word: char list = List.Empty
    let rack = getRack state pieces // Retrieve our current hand
    let initArc = state.dict

    let res = genAux state startPos pos dir word rack initArc []

    res

let getNextMove (st: state) (pieces: Map<uint32, tile>) =

    let createAsyncMoveCalculation coord dir  =
        async {
            let result = gen st pieces coord dir
            return result;
        } 

    let asyncCalculation = 
        st.tilePlacement
        |> Map.toList 
        |> List.fold (fun acc (coord, _) -> [(createAsyncMoveCalculation coord Horizontal)] @ acc |> (@) [(createAsyncMoveCalculation coord Vertical)]) List.Empty
        |> Async.Parallel
    
    let possibleWords = 
        Async.RunSynchronously asyncCalculation
        |> Array.filter (fun word -> not <| List.isEmpty word)
    
    if possibleWords.Length = 0 then List.Empty
    else possibleWords[0]
