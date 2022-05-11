module internal CrossCheck

open State
open Play
open MoveUtil
open ScrabbleUtil

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Set.ofSeq

let getLetters (dict: Dictionary.Dict) =

    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Seq.toList

    let rec aux (dict: Dictionary.Dict) (letters: List<char>) (allowed: Set<char>) =
        match letters with
        | [] -> allowed
        | letter :: letters ->
            match Dictionary.step letter dict with
            | Some _ -> aux dict letters (Set.add letter allowed)
            | None -> aux dict letters allowed

    aux dict alphabet Set.empty

let getAllowedLetters (st: state) (coord: coord) : Set<char> =
    Map.tryFind coord st.crossCheck
    |> Option.defaultValue alphabet

let determineLetterDirection (word: List<Move>) : Direction =
    match word.Length with
    | 1 -> Horizontal
    | _ ->
        match (List.item 1 word |> fst |> fst)
              - ((word.Head |> fst) |> fst)
            with
        | 0 -> Horizontal
        | _ -> Vertical

let getReverseDirection (direction: Direction) =
    match direction with
    | Horizontal -> Vertical
    | Vertical -> Horizontal

let goBackwardsOffset offset direction =
    match direction with
    | Horizontal -> offset + 1
    | Vertical -> offset - 1

let goForwardsOffset offset direction =
    match direction with
    | Horizontal -> offset - 1
    | Vertical -> offset + 1

let getLetterFromBoardOrMoves (st: state) (coord: coord) (moves: Move list) : Option<Tile> =
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

let findWordEnd (st: state) (moves: Move list) (anchor: coord) (direction: Direction) =
    let rec aux offset =
        match getLetterFromBoardOrMoves st (getNextCoordinate anchor offset direction) moves with
        | Some _ -> aux (goBackwardsOffset offset direction)
        | None -> getNextCoordinate anchor (goForwardsOffset offset direction) direction

    aux 0

let goToWordStart (st: state) (moves: Move list) (anchor: coord) (direction: Direction) =
    let rec aux offset dict =
        match getLetterFromBoardOrMoves st (getNextCoordinate anchor offset direction) moves with
        | Some (_, (c, _)) ->
            match Dictionary.step c dict with
            | Some (_, newDict) -> aux (goForwardsOffset offset direction) newDict
            | None -> failwith "Invalid move"
        | None -> (getNextCoordinate anchor (goBackwardsOffset offset direction) direction, dict)

    aux 0 st.dict

let getAllowedDictLetters (dict: Dictionary.Dict) (allowedLetters: Set<char>) =
    let rec aux (dict: Dictionary.Dict) (letters: char list) (allowed: Set<char>) =
        match letters with
        | [] -> allowed
        | letter :: letters ->
            match Dictionary.step letter dict with
            | Some _ -> aux dict letters (Set.add letter allowed)
            | None -> aux dict letters allowed

    aux dict (Set.toList allowedLetters) Set.empty

// For each coord:
//   - Remove it from the board
//   - Check vertically down until you hit a blank spot
//      - Go throught the gaddag up until you hit a blank spot, the allowed letters on the board are the ones that are in the gaddag there
//      - The allowed letters on the bottom are the ones that are in the after reversing the direction
//   - Do the same horizontally going to the right first
let update (word: List<Move>) (st: state) : state =
    let auxHandleWord dir play map =
        let coord = fst play

        let wordEnd = findWordEnd st word coord dir
        let wordStart = goToWordStart st word wordEnd dir

        let wordStartP1Coord =
            getNextCoordinate (fst wordStart) (goForwardsOffset 0 dir) dir

        let wordEndP1Coord = getNextCoordinate wordEnd (goBackwardsOffset 0 dir) dir

        let map' =
            map
            |> Map.add coord Set.empty
            |> Map.add
                wordStartP1Coord
                (getAllowedDictLetters (wordStart |> snd) (getAllowedLetters st wordStartP1Coord))

        match Dictionary.reverse (wordStart |> snd) with
        | Some (_, newDict) ->
            Map.add wordEndP1Coord (getAllowedDictLetters newDict (getAllowedLetters st wordStartP1Coord)) map'
        | None -> Map.add wordEndP1Coord Set.empty map'

    let rec aux word dir map =
        match word with
        | [] -> map
        | play :: word' -> map |> auxHandleWord dir play |> aux word' dir

    let direction = determineLetterDirection word
    let reverseDirection = getReverseDirection direction

    { st with
        crossCheck =
            st.crossCheck
            |> aux word reverseDirection
            |> auxHandleWord direction (List.head word) }
