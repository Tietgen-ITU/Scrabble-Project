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
    | Blank _ -> not (allowedLetters.Count = 0)

let convertToPiece pieceId a =
    match pieceId with
    | 0u -> Blank(pieceId, Map.ofSeq a)
    | _ -> Normal(pieceId, Seq.head a)
