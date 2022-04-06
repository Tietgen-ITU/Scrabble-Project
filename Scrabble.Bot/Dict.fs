module Dictionary

open System.Collections.Generic

type Dict = D of char * Dictionary<char, Dict> * bool

let sep = '$'

type Dictionary<'a, 'b> with
    member x.GetValueOption(key: 'a) : 'b option =
        match x.TryGetValue(key) with
        | (true, value) -> Some(value)
        | (false, _) -> None

let emptyDictionary () : Dictionary<char, Dict> = Dictionary()

let empty () : Dict = D(' ', emptyDictionary (), false)

let getNode (w: char) (D (_, children, _)) : Dict option = children.GetValueOption(w)

let rec actuallyInsert (word: char list) (D (currC, currChildren, currIsLeaf)) : Dict =
    match word with
    | [] -> D(currC, currChildren, true)
    | nextChar :: nextWord ->
        let nextNode =
            match currChildren.GetValueOption(nextChar) with
            | Some node -> node
            | None -> D(nextChar, emptyDictionary (), false)

        currChildren.[nextChar] <- actuallyInsert nextWord nextNode

        D(currC, currChildren, currIsLeaf)

let insert (word: string) (dict: Dict) : Dict =
    let arr = word.ToCharArray()

    let mutable newDict = dict

    for i in 1 .. arr.Length do
        let prefix, suffix = Array.splitAt i arr

        let w =
            (prefix |> Array.rev |> Array.toList)
            @ [ sep ] @ (suffix |> Array.toList)

        newDict <- actuallyInsert w newDict

    newDict

let rec step (c: char) (dict: Dict) : (bool * Dict) option =
    getNode c dict
    |> Option.map (fun node ->
        match node with
        | D (_, _, isLeaf) -> (isLeaf, node))

let reverse (dict: Dict) : (bool * Dict) option = step sep dict

let rec actuallyLookup (word: char list) (dict: Dict) : bool =
    match word with
    | [] ->
        match dict with
        | D (_, _, isLeaf) -> isLeaf
    | c :: w ->
        match step c dict with
        | Some (_, node) -> actuallyLookup w node
        | None ->
            match reverse dict with
            | Some (_, node) -> actuallyLookup ([ c ] @ w) node
            | None -> false


let lookup (word: string) (dict: Dict) : bool =
    let (pre, suff) =
        word.ToCharArray()
        |> Array.toList
        |> List.splitAt 1

    actuallyLookup (pre @ [ sep ] @ suff) dict
