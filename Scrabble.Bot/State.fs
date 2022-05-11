module internal State

open ScrabbleUtil

// Make sure to keep your state localised in this module. It makes your life a whole lot easier.
// Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
// but it could, potentially, keep track of other useful
// information, such as number of players, player turn, etc.

type state =
    { board: Parser.board
      dict: Dictionary.Dict
      playerId: uint32
      points: List<int>
      hand: MultiSet.MultiSet<uint32>
      players: List<bool>
      playerTurn: uint32
      crossCheck: Map<coord, Set<char>>
      tilePlacement: Map<coord, uint32 * (char * int)> }

let mkState b d pn h pl pt =
    { board = b
      dict = d
      playerId = pn
      hand = h
      players = pl
      playerTurn = pt
      tilePlacement = Map.empty<coord, uint32 * (char * int)>
      points = [ for _ in 1 .. pl.Length -> 0 ] 
      crossCheck = Map.empty }

type Direction =
    | Horizontal
    | Vertical

let removePlayer st playerIdToRemove =
    { st with players = List.updateAt (playerIdToRemove - 1) false st.players }

let board st = st.board
let dict st = st.dict
let playerId st = st.playerId
let hand st = st.hand
let players st = st.players
let playerTurn st = st.playerTurn

let removeTileFromHand st tileId =
    { st with hand = MultiSet.removeSingle tileId st.hand }

let removeTilesFromHand (tileIds: List<uint32>) st = List.fold removeTileFromHand st tileIds

let addTileToHand st (tileId, amount: uint32) =
    let rec aux ms id =
        function
        | 0u -> ms
        | x -> aux (MultiSet.addSingle id ms) tileId (x - 1u)

    { st with hand = (aux st.hand tileId amount) }

let addPoints (playerId: uint32) points st =
    let playerId = int playerId - 1
    { st with points = List.updateAt playerId (points + st.points.Item(playerId)) st.points }

let private (|FoundValue|_|) key map = Map.tryFind key map

let placeLetter (tile: uint32 * (char * int)) (st: state) (coordinate: coord) =
    match st.tilePlacement with
    | FoundValue coordinate _ -> failwith "There is already a tile placed at that coordinate"
    | _ -> { st with tilePlacement = Map.add coordinate tile st.tilePlacement }

let placeLetters tiles =
    Seq.foldBack (fun (coord, tile) acc -> placeLetter tile acc coord) tiles

let hasLetter coordinate =
    function
    | FoundValue coordinate _ -> true
    | _ -> false

let hasSquare (board: Parser.board) coord =
    match board.squares coord with
    | StateMonad.Success a ->
        match a with
        | Some _ -> true
        | None -> false
    | StateMonad.Failure _ -> false

let isBeginingOfWord pos orientation st =
    let adjacentCoord =
        match orientation with
        | Vertical -> (pos |> fst, (pos |> snd) + 1)
        | Horizontal -> ((pos |> fst) - 1, pos |> snd)

    not (hasSquare st.board adjacentCoord)
    || not (hasLetter adjacentCoord st.tilePlacement)

(*
    Updates the board with the function provided. This is created as there are a lot of different
    functions that both parses the board and does other stuff. So this is created as a more general purpose.
    NOTE: We could hide this function and create more specific functions if we want to.
*)
let updateBoard f st = { st with board = f st.board }

let playerIsActive (st: state) = st.players.Item(int st.playerTurn - 1)

let rec changeTurn (st: state) =
    let aux (playerTurn: uint32) =
        let st = { st with playerTurn = playerTurn }

        match playerIsActive st with
        | true -> st
        | false -> changeTurn st

    match st.playerTurn >= uint32 st.players.Length with
    | true -> aux 1u
    | false -> aux (st.playerTurn + 1u)
