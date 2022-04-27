module internal State

open ScrabbleUtil

// Make sure to keep your state localised in this module. It makes your life a whole lot easier.
// Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
// but it could, potentially, keep track of other useful
// information, such as number of players, player turn, etc.

type state =
    { board: Parser.board
      dict: Dictionary.Dict
      playerId: uint32
      points: int
      hand: MultiSet.MultiSet<uint32>
      players: List<bool>
      playerTurn: uint32
      tilePlacement: Map<coord, (char * int)> }

let mkState b d pn h pl pt =
    { board = b
      dict = d
      playerId = pn
      hand = h
      players = pl
      playerTurn = pt
      tilePlacement = Map.empty<coord, (char * int)>
      points = 0 }

let removePlayer st playerIdToRemove =
    { st with players = List.updateAt (playerIdToRemove - 1) false st.players }

let board st = st.board
let dict st = st.dict
let playerId st = st.playerId
let hand st = st.hand
let players st = st.players
let playerTurn st = st.playerTurn

let removeTileFromHand st tileId =
    { st with hand = st.hand.Remove tileId }

let addTileToHand st tileId value =
    { st with hand = st.hand.Add(tileId, value) }

let addPoints playerId points st =
    if st.playerId = playerId then
        { st with points = st.points + points }
    else
        st

let private (|FoundValue|_|) key map = Map.tryFind key map

let placeLetter (tile: (char * int)) (st: state) (coordinate: coord) =
    // TODO: Place them on the board as well... and not only in the new map
    match st.tilePlacement with
    | FoundValue coordinate _ -> failwith "There is already a tile placed at that coordinate"
    | _ -> { st with tilePlacement = Map.add coordinate tile st.tilePlacement }

let placeLetters tiles =
    Seq.foldBack (fun (coord, tile) acc -> placeLetter tile acc coord) tiles

let hasLetter coordinate =
    function
    | FoundValue coordinate _ -> true
    | _ -> false

(*
        Updates the board with the function provided. This is created as there are a lot of different
        functions that both parses the board and does other stuff. So this is created as a more general purpose.

        NOTE: We could hide this function and create more specific functions if we want to.
    *)
let updateBoard f st = { st with board = f st.board }

let playerIsActive (st: state) = st.players.Item(int st.playerTurn - 1)

let rec changeTurn (st: state) =
    match st.playerTurn >= uint32 st.players.Length with
    | true ->
        if playerIsActive { st with playerTurn = 1u } then
            { st with playerTurn = 1u }
        else
            changeTurn { st with playerTurn = 1u }
    | false ->
        if playerIsActive { st with playerTurn = (st.playerTurn + 1u) } then
            { st with playerTurn = (st.playerTurn + 1u) }
        else
            changeTurn { st with playerTurn = (st.playerTurn + 1u) }
