module internal Rack

open ScrabbleUtil

open State
open Piece

let getRack (state: State.state) (pieces: Map<uint32, tile>) : Piece list =
    let findPiece piece pieces =
        Map.find piece pieces
        |> Set.toSeq
        |> convertToPiece piece

    let rec createPieces rack piece pieces =
        function
        | 0u -> rack
        | x -> createPieces ((findPiece piece pieces) :: rack) piece pieces (x - 1u)

    state.hand
    |> MultiSet.fold (fun rack piece -> createPieces rack piece pieces) []
