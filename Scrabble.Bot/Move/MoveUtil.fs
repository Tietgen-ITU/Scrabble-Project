module internal MoveUtil

open System.Text

open ScrabbleUtil
open State
open Play

let listToString (chars: Tile list) =
    string (List.fold (fun (sb: StringBuilder) c -> sb.Append(char (c |> snd |> fst))) (StringBuilder()) chars)

let playListToString (plays: Play list) : string =
    plays
    |> getNormalWord
    |> List.map snd
    |> listToString

let playsToString (plays: Plays) : string = plays |> fst |> playListToString

let returnPlays (plays: Plays) : Plays = ([], plays |> snd)

let testCoord (st: state) (coord: coord) = hasLetter coord st.tilePlacement

let getLetter coordinate (state: State.state) =
    match state.tilePlacement.TryFind coordinate with
    | Some c -> Some c
    | None -> None

let nextArc (arc: Dictionary.Dict) (c: Tile) = Dictionary.step (c |> snd |> fst) arc

let getNextCoordinate (pos: coord) (offset: int32) (dir: Direction) =
    match dir with
    | Vertical -> (pos |> fst, (pos |> snd) + offset)
    | Horizontal -> ((pos |> fst) + offset, pos |> snd)

// FIXME: Might not be the best place for this, but it's used by the Mailbox.fs file
let getBestMove (state: State.state) (currentBest: int * Play list) (newPlay: Play list) =
    let newPoints =
        DIB.PointCalculator.calculateWordPoint (getNormalWord newPlay) state.board

    match newPoints > (fst currentBest) with
    | true -> (newPoints, newPlay)
    | false -> currentBest

let switchDirection f (defaultValue: 'a) (dict: Dictionary.Dict) : 'a =
    match Dictionary.reverse dict with
    | Some (valid, newArc) -> f valid newArc
    | None -> defaultValue
