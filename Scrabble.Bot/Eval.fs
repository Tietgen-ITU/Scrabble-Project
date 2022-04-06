// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

open StateMonad

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

let binop f a b =
    a >>= (fun x -> b >>= fun y -> ret (f x y))

let add a b = binop (+) a b

let div a b =
    a
    >>= (fun x ->
        b
        >>= fun y ->
                match y with
                | v when v = 0 -> fail DivisionByZero
                | v -> ret (x / v))

let isVowel c =
    match System.Char.ToUpper c with
    | 'A'
    | 'E'
    | 'I'
    | 'O'
    | 'U' -> true
    | _ -> false

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V str -> lookup str
    | WL -> wordLength
    | PV a -> arithEval a >>= pointValue
    | Add (a, b) -> add (arithEval a) (arithEval b)
    | Sub (a, b) -> binop (-) (arithEval a) (arithEval b)
    | Mul (a, b) -> binop (*) (arithEval a) (arithEval b)
    | Div (a, b) -> div (arithEval a) (arithEval b)
    | Mod (a, b) ->
        arithEval a
        >>= (fun x ->
            arithEval b
            >>= fun y ->
                    match y with
                    | v when v = 0 -> fail DivisionByZero
                    | v -> ret (x % v))
    | CharToInt c ->
        charEval c
        >>= fun x -> ret (System.Convert.ToInt32 x)

and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV a -> arithEval a >>= characterValue
    | ToUpper c ->
        charEval c
        >>= fun a -> ret (System.Char.ToUpper a)
    | ToLower c ->
        charEval c
        >>= fun a -> ret (System.Char.ToLower a)
    | IntToChar a ->
        arithEval a
        >>= fun b -> ret (System.Convert.ToChar b)

let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false

    | AEq (a, b) -> binop (=) (arithEval a) (arithEval b)
    | ALt (a, b) -> binop (<) (arithEval a) (arithEval b)

    | Not b -> boolEval b >>= fun e -> ret (not (e))
    | Conj (a, b) -> binop (&&) (boolEval a) (boolEval b)

    | IsVowel c -> charEval c >>= fun a -> ret (isVowel a)
    | IsLetter c ->
        charEval c
        >>= fun a -> ret (System.Char.IsLetter a)
    | IsDigit c ->
        charEval c
        >>= fun a -> ret (System.Char.IsDigit a)


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
    | Declare str -> declare str
    | Ass (str, exp) ->
        declare str >>>= arithEval exp
        >>= (fun vl -> update str vl)
    | Skip -> ret ()
    | Seq (s1, s2) -> stmntEval s1 >>>= stmntEval s2
    | ITE (exp, s1, s2) ->
        boolEval exp
        >>= fun b -> if b then stmntEval s1 else stmntEval s2
    | While (exp, stm) ->
        boolEval exp
        >>= fun b ->
                if b then
                    stmntEval stm >>>= stmntEval stmnt
                else
                    ret ()

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let rec arithEval2 a =
    match a with
    | N n -> prog { return n }
    | V str -> prog { return! lookup str }
    | WL -> prog { return! wordLength }
    | PV a ->
        prog {
            let! x = arithEval a
            return! pointValue x
        }
    | Add (a, b) -> prog { return! add (arithEval2 a) (arithEval2 a) }
    | Sub (a, b) -> prog { return! binop (-) (arithEval2 a) (arithEval2 a) }
    | Mul (a, b) -> prog { return! binop (*) (arithEval2 a) (arithEval2 a) }
    | Div (a, b) -> prog { return! div (arithEval2 a) (arithEval2 a) }
    | Mod (a, b) ->
        prog {
            let! x = arithEval2 a
            let! y = arithEval2 b

            if y = 0 then
                return! fail DivisionByZero
            else
                return (x % y)
        }
    | CharToInt c ->
        prog {
            let! x = charEval2 c
            return System.Convert.ToInt32((char) x)
        }

and charEval2 c =
    match c with
    | C c -> prog { return c }
    | CV a ->
        prog {
            let! x = arithEval2 a
            return! characterValue x
        }
    | ToUpper c ->
        prog {
            let! x = charEval2 c
            return System.Char.ToUpper x
        }
    | ToLower c ->
        prog {
            let! x = charEval2 c
            return System.Char.ToLower x
        }
    | IntToChar a ->
        prog {
            let! x = arithEval2 a
            return System.Convert.ToChar x
        }

let rec boolEval2 b =
    match b with
    | TT -> prog { return true }
    | FF -> prog { return true }

    | AEq (a, b) -> prog { return! binop (=) (arithEval2 a) (arithEval2 b) }
    | ALt (a, b) -> prog { return! binop (<) (arithEval2 a) (arithEval2 b) }

    | Not b ->
        prog {
            let! x = boolEval2 b
            return (not (x))
        }
    | Conj (a, b) -> prog { return! binop (&&) (boolEval2 a) (boolEval2 b) }

    | IsVowel c ->
        prog {
            let! x = charEval2 c
            return isVowel x
        }
    | IsLetter c ->
        prog {
            let! x = charEval2 c
            return (System.Char.IsLetter x)
        }
    | IsDigit c ->
        prog {
            let! x = charEval2 c
            return (System.Char.IsDigit x)
        }

let rec stmntEval2 stm =
    match stm with
    | Declare str -> prog { return! declare str }
    | Ass (str, exp) ->
        prog {
            let _ = declare str
            let! x = arithEval2 exp
            return! update str x
        }
    | Skip -> prog { return () }
    | Seq (s1, s2) ->
        prog {
            let! x = stmntEval2 s1
            return! stmntEval2 s2
        }
    | ITE (exp, s1, s2) ->
        prog {
            let! b = boolEval2 exp

            if b then
                return! stmntEval2 s1
            else
                return! stmntEval2 s2
        }
    | While (exp, stm) ->
        prog {
            let! b = boolEval2 exp

            if b then
                let _ = stmntEval2 stm
                return! stmntEval2 stm
            else
                return ()
        }

(* Part 4 *)

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm : squareFun =
    fun word pos acc ->
        prog {
            let state =
                mkState
                    [ ("_pos_", pos)
                      ("_acc_", acc)
                      ("_result_", 0) ]
                    word
                    [ "_pos_"; "_acc_"; "_result_" ]

            return!
                stmntEval2 stm >>>= lookup "_result_"
                |> evalSM state
        }


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
