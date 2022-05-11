module internal Piece

type Piece =
    | Normal of (uint32 * (char * int))
    | Blank of (uint32 * Map<char, int>)

let isBlank =
    function
    | Normal _ -> false
    | Blank _ -> true

let hasBlank = List.exists isBlank

let getNormalPiece =
    function
    | Normal a -> a
    | _ -> failwith "this is not a normal piece"

let getPieceAsBlank =
    function
    | Normal _ -> failwith "this is not a blank piece"
    | Blank a -> a

let pieceIsAllowed allowedLetters =
    function
    | Normal (_, (ch, _)) -> Set.contains ch allowedLetters
    | Blank _ -> true

(*
    Provides with a list of normal pieces.
    If the Piece is of type Blank then the blank will return a list of normal pieces with all the letters in the allowedLetters
*)
let getAllowedPieces allowedPieces =
    function
    | Normal a -> [ Normal a ]
    | Blank (id, _) -> Set.fold (fun acc piece -> Normal(id, (piece, 0)) :: acc) List.Empty allowedPieces
