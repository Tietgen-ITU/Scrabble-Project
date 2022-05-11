module internal Rack

open ScrabbleUtil

open State
open Piece

let getRack (state: State.state) (pieces: Map<uint32, tile>) : Piece list =
    let convertToPiece pieceId a =
        match pieceId with
        | 0u -> Blank(pieceId, Map.ofSeq a)
        | _ -> Normal(pieceId, Seq.head a)

    let rec createPieces rack piece pieces =
        function
        | 0u -> rack
        | x ->
            createPieces
                ((Map.find piece pieces
                  |> Set.toSeq
                  |> convertToPiece piece)
                 :: rack)
                piece
                pieces
                (x - 1u)

    state.hand
    |> MultiSet.fold (fun rack piece -> createPieces rack piece pieces) []

let getAllowedLetters (dict: Dictionary.Dict) =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Seq.toList

    let rec aux (dict: Dictionary.Dict) (letters: char list) (allowed: Set<char>) =
        match letters with
        | [] -> allowed
        | letter :: letters ->
            match Dictionary.step letter dict with
            | Some _ -> aux dict letters (Set.add letter allowed)
            | None -> aux dict letters allowed

    aux dict alphabet Set.empty
