module StateMonad

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b

type Error =
    | VarExists of string
    | VarNotFound of string
    | IndexOutOfBounds of int
    | DivisionByZero
    | ReservedName of string
    | EmptyStack

type State =
    { vars: Map<string, int> list
      word: (char * int) list
      reserved: Set<string> }

type SM<'a> = S of (State -> Result<'a * State, Error>)

let mkState lst word reserved =
    { vars = [ Map.ofList lst ]
      word = word
      reserved = Set.ofList reserved }

let evalSM (s: State) (S a: SM<'a>) : Result<'a, Error> =
    match a s with
    | Success (result, _) -> Success result
    | Failure error -> Failure error

let bind (f: 'a -> SM<'b>) (S a: SM<'a>) : SM<'b> =
    S (fun s ->
        match a s with
        | Success (b, s') ->
            match f b with
            | S g -> g s'
        | Failure err -> Failure err)


let ret (v: 'a) : SM<'a> = S(fun s -> Success(v, s))
let fail err : SM<'a> = S(fun _ -> Failure err)

let (>>=) x f = bind f x
let (>>>=) x f = x >>= (fun () -> f)

let push: SM<unit> = S(fun s -> Success((), { s with vars = Map.empty :: s.vars }))

let pop: SM<unit> = S(fun s -> Success((), { s with vars = s.vars.Tail }))

let wordLength: SM<int> = S(fun s -> Success(s.word.Length, s))

let getCharacter (pos: int) (f: char * int -> 'a) : SM<'a> =
    S (fun s ->
        if pos >= s.word.Length || pos < 0 then
            Failure(IndexOutOfBounds pos)
        else
            Success(s.word[pos] |> f, s))

let characterValue (pos: int) : SM<char> = getCharacter pos fst

let pointValue (pos: int) : SM<int> = getCharacter pos snd

let lookup (x: string) : SM<int> =
    let rec aux =
        function
        | [] -> None
        | m :: ms ->
            match Map.tryFind x m with
            | Some v -> Some v
            | None -> aux ms

    S (fun s ->
        match aux s.vars with
        | Some v -> Success(v, s)
        | None -> Failure(VarNotFound x))

let declare (var: string) : SM<unit> =
    let isReserved (var: string) (state: State) =
        match state.reserved.Contains var with
        | true -> Failure(ReservedName var)
        | false -> Success((), state)

    let stackEmpty (state: State) : Result<Map<string, int> * Map<string, int> list, Error> =
        match state.vars with
        | [] -> Failure(EmptyStack)
        | xs :: x -> Success((xs, x))

    let exists (var: string) (state: State) =
        match state.vars.Head.ContainsKey var with
        | true -> Failure(VarExists var)
        | false -> Success((), state)

    S (fun s ->
        match isReserved var s with
        | Success _ ->
            match stackEmpty s with
            | Success (x, xs) ->
                match exists var s with
                | Success _ -> Success((), { s with vars = x.Add(var, 0) :: xs })
                | Failure err -> Failure err
            | Failure err -> Failure err
        | Failure err -> Failure err)


let update (var: string) (value: int) : SM<unit> =
    let rec aux (values: Map<string, int> list) (x: string) (value: int) =
        match values with
        | [] -> Failure(VarNotFound var)
        | m :: ms ->
            match m.ContainsKey x with
            | true -> Success((), m.Add(x, value) :: ms)
            | false ->
                match aux ms x value with
                | Success ((), s) -> Success((), m :: s)
                | Failure err -> Failure(err)

    S (fun s ->
        match aux s.vars var value with
        | Success ((), b) -> Success((), { s with vars = b })
        | Failure err -> Failure(err))
