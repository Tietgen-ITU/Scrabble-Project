module internal CrossCheck

open State
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

let getAllowedLetters (st: state) (coord: coord): Set<char> =
    Map.tryFind coord st.crossCheck
    |> Option.defaultValue alphabet