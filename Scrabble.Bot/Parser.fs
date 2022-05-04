// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

open StateMonad
open ScrabbleUtil // NEW. KEEP THIS LINE.
open System
open Eval
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.


let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"

let palphanumeric =
    satisfy System.Char.IsLetterOrDigit
    <?> "alphanumeric"

let spaces = many whitespaceChar <?> "spaces"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) a b = (a .>> spaces) .>>. b
let (.>*>) a b = (a .>> spaces) .>> b
let (>*>.) a b = (a .>> spaces) >>. b

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
let brackethise p = pchar '{' >*>. p .>*> pchar '}'

let pid =
    (pchar '_' <|> pletter)
    .>>. many (palphanumeric <|> pchar '_')
    |>> fun (a, b) -> a :: b |> List.toArray |> System.String.Concat


let unop a b = a >*>. b
let binop op p1 p2 = p1 .>*> op .>*>. p2

let methodOp (method: Parser<'a>) (argParser: Parser<'b>) =
    unop method (parenthesise argParser)
    <?> "methodOp"

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CharParse, cref = createParserForwardedToRef<cExp> ()

let AddParse =
    binop (pchar '+') ProdParse TermParse |>> Add
    <?> "Add"

let SubParse =
    binop (pchar '-') ProdParse TermParse |>> Sub
    <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse =
    binop (pchar '*') AtomParse ProdParse |>> Mul
    <?> "Mul"

let DivParse =
    binop (pchar '/') AtomParse ProdParse |>> Div
    <?> "Div"

let ModParse =
    binop (pchar '%') AtomParse ProdParse |>> Mod
    <?> "Mod"

do
    pref
    := choice [ MulParse
                DivParse
                ModParse
                AtomParse ]


let NParse = pint32 |>> N <?> "Int"
let ParParse = parenthesise TermParse

let NegParse =
    unop (pchar '-') TermParse
    |>> (fun a -> Mul(N -1, a))
    <?> "Neg"

let PVParse = unop pPointValue ParParse |>> PV <?> "PV"
let VarParse = pid |>> V <?> "Variable"

let CharToIntParse =
    unop pCharToInt (parenthesise CharParse)
    |>> CharToInt
    <?> "CharToInt"

do
    aref
    := choice [ NegParse
                PVParse
                CharToIntParse
                VarParse
                NParse
                ParParse ]

let AexpParse = TermParse

let CParse =
    between (pchar ''') (pchar ''') anyChar |>> C
    <?> "Char"

let CVParse =
    unop pCharValue (parenthesise TermParse) |>> CV
    <?> "CharValue"

let ITCParse =
    unop pIntToChar (parenthesise TermParse)
    |>> IntToChar
    <?> "IntToChar"

let TUParse =
    unop pToUpper (parenthesise CharParse) |>> ToUpper
    <?> "ToUpper"

let TLParse =
    unop pToLower (parenthesise CharParse) |>> ToLower
    <?> "ToLower"

do
    cref
    := choice [ CVParse
                ITCParse
                TUParse
                TLParse
                CParse ]

let CexpParse = CharParse

let ConjunctionParse, conref = createParserForwardedToRef<bExp> ()
let EqualityParse, eqref = createParserForwardedToRef<bExp> ()
let BMethodParse, bmref = createParserForwardedToRef<bExp> ()

let ConjParse =
    binop (pstring "/\\") EqualityParse ConjunctionParse
    |>> Conj
    <?> "Conjunction"

let CreateDisj (a: bExp, b: bExp) = (Not a, Not b) |> Conj |> Not

let DisjParse =
    binop (pstring "\\/") EqualityParse ConjunctionParse
    |>> (fun x -> CreateDisj x)
    <?> "Disjunction"

do
    conref
    := choice [ ConjParse
                DisjParse
                EqualityParse ]

let AEqParse =
    binop (pchar '=') AexpParse AexpParse |>> AEq
    <?> "Equal"

let ANEqParse =
    binop (pstring "<>") AexpParse AexpParse
    |>> (fun x -> x |> AEq |> Not)
    <?> "NotEqual"

let ALtParse =
    binop (pchar '<') AexpParse AexpParse |>> ALt
    <?> "LessThan"

// This works, but it's as cursed as it gets
let ALtOrEqParse =
    binop (pstring "<=") AexpParse AexpParse
    |>> (fun x -> (ALt x, AEq x |> Not |> Not) |> CreateDisj)
    <?> "LessThanOrEqual"

let AGtParse =
    binop (pchar '>') AexpParse AexpParse
    |>> (fun x -> (AEq x |> Not, ALt x |> Not) |> Conj)
    <?> "GreaterThan"

let AGtOrEqParse =
    binop (pstring ">=") AexpParse AexpParse
    |>> (fun x -> ALt x |> Not)
    <?> "GreaterThanOrEqual"


do
    eqref
    := choice [ AEqParse
                ANEqParse
                ALtParse
                ALtOrEqParse
                AGtParse
                AGtOrEqParse
                BMethodParse ]

let NotParse = pchar '~' >>. ConjunctionParse |>> Not <?> "Not"

let IsDigitParse =
    methodOp pIsDigit CexpParse |>> IsDigit
    <?> "IsDigit"

let IsLetterParser =
    methodOp pIsDigit CexpParse |>> IsLetter
    <?> "IsLetter"

let IsVowellParse =
    methodOp pIsDigit CexpParse |>> IsVowel
    <?> "IsVowel"

let TTParse = pTrue |>> (fun _ -> TT) <?> "True"
let FFParse = pFalse |>> (fun _ -> FF) <?> "False"

let ParBParse = parenthesise ConjunctionParse <?> "BParentheses"

do
    bmref
    := choice [ NotParse
                IsDigitParse
                IsLetterParser
                IsVowellParse
                TTParse
                FFParse
                ParBParse ]

let BexpParse = ConjunctionParse

let TopLevelParser, tsref = createParserForwardedToRef<stm> ()
let StatementParser, sref = createParserForwardedToRef<stm> ()

let SemicolonParser =
    StatementParser .>*> pchar ';'
    .>*>. TopLevelParser
    |>> Seq
    <?> "Semicolon"

do
    tsref
    := choice [ SemicolonParser
                StatementParser ]

let AssignParser =
    pid .>*> pstring ":=" .>*>. AexpParse |>> Ass
    <?> "Assign"

let DeclareParser =
    pdeclare >>. spaces1 >>. pid |>> Declare
    <?> "Declare"

let getIf =
    methodOp pif BexpParse .>*> pthen
    .>*>. brackethise StatementParser

let IfParser =
    getIf |>> (fun (b, s1) -> ITE(b, s1, Skip))
    <?> "If"

let IfElseParser =
    getIf .>*> pelse .>*>. brackethise StatementParser
    |>> (fun ((b, s1), s2) -> ITE(b, s1, s2))
    <?> "IfElse"

let WhileParser =
    methodOp pwhile BexpParse .>*> pdo
    .>*>. brackethise StatementParser
    |>> While
    <?> "While"

do
    sref
    := choice [ AssignParser
                DeclareParser
                IfElseParser
                IfParser
                WhileParser ]

let stmntParse = TopLevelParser

(* The rest of your parser goes here *)

type word = (char * int) list
// word - pos in word - acc of points - result
type squareFun = word -> int -> int -> Result<int, Error>
type square = Map<int, squareFun>

type boardFun2 = coord -> Result<square option, Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

let parseSquareProg (sqp: squareProg) : square =
    Map.map
        (fun _ v ->
            v
            |> run stmntParse
            |> getSuccess
            |> stmntToSquareFun)
        sqp

let parseBoardProg (s: string) (sqs: Map<int, square>) : boardFun2 =
    run stmntParse s
    |> getSuccess
    |> (fun x -> stmntToBoardFun x sqs)

let mkBoard (bp: boardProg) : board =
    let m' = Map.map (fun _ v -> parseSquareProg v) bp.squares

    { center = bp.center
      defaultSquare = m'.[bp.usedSquare]
      squares = parseBoardProg bp.prog m' }
