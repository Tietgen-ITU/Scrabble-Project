module internal Eval

open StateMonad

(* Code for testing *)

let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let state =
    mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]

let emptyState = mkState [] [] []

let arith (a: SM<int>) (b: SM<int>) (f: (int -> int -> int)) =
    a >>= (fun x -> b >>= (fun y -> ret (f x y)))

let add (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x + y)
let sub (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x - y)
let mul (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x * y)

let arith0Sens (a: SM<int>) (b: SM<int>) (f: (int -> int -> int)) =
    a
    >>= (fun x ->
        b
        >>= (fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                ret (f x y)))

let div a b = arith0Sens a b (fun x y -> x / y)
let modulo a b = arith0Sens a b (fun x y -> x % y)

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V v -> lookup v
    | WL -> wordLength
    | PV p -> arithEval p >>= (fun x -> pointValue x)
    | Add (a, b) -> add (arithEval a) (arithEval b)
    | Sub (a, b) -> sub (arithEval a) (arithEval b)
    | Mul (a, b) -> mul (arithEval a) (arithEval b)
    | Div (a, b) -> div (arithEval a) (arithEval b)
    | Mod (a, b) -> modulo (arithEval a) (arithEval b)
    | CharToInt (c) -> charEval c >>= (fun x -> ret (int x))

and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV v -> arithEval v >>= (fun x -> characterValue x)
    | ToUpper c ->
        charEval c
        >>= (fun x -> ret (System.Char.ToUpper x))
    | ToLower c ->
        charEval c
        >>= (fun x -> ret (System.Char.ToLower x))
    | IntToChar i -> arithEval i >>= (fun x -> ret (char x))

let numericComparison (a: aExp) (b: aExp) (f: (int -> int -> bool)) : SM<bool> =
    arithEval a
    >>= (fun x -> arithEval b >>= (fun y -> ret (f x y)))

let isVowel (c: char) : bool =
    match System.Char.ToLower(c) with
    | 'a'
    | 'e'
    | 'i'
    | 'o'
    | 'u' -> true
    | _ -> false


let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (a, b) -> numericComparison a b (fun x y -> x = y)
    | ALt (a, b) -> numericComparison a b (fun x y -> x < y)
    | Not b -> boolEval b >>= (fun x -> ret (not x))
    | Conj (b1, b2) ->
        boolEval b1
        >>= (fun x -> boolEval b2 >>= (fun y -> ret (x && y)))
    | IsVowel c -> charEval c >>= (fun x -> ret (isVowel x))
    | IsLetter c ->
        charEval c
        >>= (fun x -> ret (System.Char.IsLetter x))
    | IsDigit c ->
        charEval c
        >>= (fun x -> ret (System.Char.IsDigit x))


type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> =
    match stmnt with
    | Declare v -> declare v
    | Ass (v, a) -> arithEval a >>= (fun x -> update v x)
    | Skip -> ret ()
    | Seq (s1, s2) -> stmntEval s1 >>= (fun _ -> stmntEval s2)
    | ITE (b, s1, s2) ->
        boolEval b
        >>= (fun x ->
            push
            >>>= (if x then stmntEval s1 else stmntEval s2)
            >>>= pop)
    | While (b, s) ->
        boolEval b
        >>= (fun x ->
            push
            >>>= (if x then
                      stmntEval s >>= (fun _ -> stmntEval (While(b, s)))
                  else
                      ret ())
            >>>= pop)


(* Part 3 (Optional) *)
type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let rec arithEval2 (a: aExp) : SM<int> =
    prog {
        match a with
        | N n -> return n
        | V v ->
            let! x = lookup v
            return x
        | WL ->
            let! wl = wordLength
            return wl
        | PV p ->
            let! x = arithEval2 p
            let! pv = pointValue x
            return pv
        | Add (a, b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b
            return x + y
        | Sub (a, b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b
            return x - y
        | Mul (a, b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b
            return x * y
        | Div (a, b) ->
            let x = arithEval2 a
            let y = arithEval2 b
            return! arith0Sens x y (fun x y -> x / y)
        | Mod (a, b) ->
            let x = arithEval2 a
            let y = arithEval2 b
            return! arith0Sens x y (fun x y -> x % y)
        | CharToInt (c) ->
            let! x = charEval2 c
            return int x
    }

and charEval2 c =
    prog {
        match c with
        | C c -> return c
        | CV v ->
            let! x = arithEval2 v
            return! characterValue x
        | ToUpper c ->
            let! x = charEval2 c
            return System.Char.ToUpper x
        | ToLower c ->
            let! x = charEval2 c
            return System.Char.ToLower x
        | IntToChar i ->
            let! x = arithEval2 i
            return char x
    }

let rec boolEval2 b =
    prog {
        match b with
        | TT -> return true
        | FF -> return false
        | AEq (a, b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b
            return x = y
        | ALt (a, b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b
            return x < y
        | Not b ->
            let! x = boolEval2 b
            return not x
        | Conj (b1, b2) ->
            let! x = boolEval2 b1
            let! y = boolEval2 b2
            return x && y
        | IsVowel c ->
            let! x = charEval2 c
            return isVowel x
        | IsLetter c ->
            let! x = charEval2 c
            return System.Char.IsLetter x
        | IsDigit c ->
            let! x = charEval2 c
            return System.Char.IsDigit x
    }

let rec stmntEval2 stm =
    prog {
        match stm with
        | Declare v -> do! declare v
        | Ass (v, a) ->
            let! x = arithEval2 a
            do! update v x
        | Skip -> return ()
        | Seq (s1, s2) ->
            do! stmntEval2 s1
            do! stmntEval2 s2
        | ITE (b, s1, s2) ->
            let! x = boolEval2 b
            do! push

            if x then
                do! stmntEval2 s1
            else
                do! stmntEval2 s2

            do! pop
        | While (b, s) ->
            let! x = boolEval2 b

            if x then
                do! push
                do! stmntEval2 s
                do! pop
                do! stmntEval2 (While(b, s))
            else
                return ()
    }

(* Part 4 (Optional) *)

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm =
    fun w pos acc ->
        let state =
            mkState
                [ ("_pos_", pos)
                  ("_acc_", acc)
                  ("_result_", 0) ]
                w
                [ "_pos_"; "_acc_"; "_result_" ]

        stmntEval stm >>>= lookup "_result_"
        |> evalSM state


type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun (stm: stm) (squares: Map<int, 'a>) : coord -> Result<'a option, Error> =
    fun coord ->
        let state =
            mkState
                [ ("_x_", coord |> fst)
                  ("_y_", coord |> snd)
                  ("_result_", 0) ]
                []
                [ "_x_"; "_y_"; "_result_" ]

        stmntEval2 stm >>>= lookup "_result_"
        >>= (fun x ->
            match squares.TryFind x with
            | Some x -> ret (Some x)
            | None -> ret (None))
        |> evalSM state

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard (c: coord) (defaultSq: stm) (boardStmnt: stm) (ids: (int * stm) list) : board =
    { center = c
      defaultSquare = stmntToSquareFun defaultSq
      squares =
        stmntToBoardFun
            boardStmnt
            ((List.map (fun (id, sq) -> (id, stmntToSquareFun sq)) ids)
             |> Map.ofList) }