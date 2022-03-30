// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b = (a .>> spaces) .>>. b
    let (.>*>) a b  = (a .>> spaces) .>> b
    let (>*>.) a b  = (a .>> spaces) >>. b

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 

    let pid = (pchar '_' <|> pletter) 
              .>>. many (palphanumeric <|> pchar '_') 
              |>> fun (a, b) -> a :: b |> List.toArray |> System.String.Concat

    
    let unop a b = a >*>. b
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]


    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') TermParse |>> (fun a -> Mul(N -1, a)) <?> "Neg"
    let PVParse  = unop pPointValue ParParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "Variable"
    let CharToIntParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; PVParse; CharToIntParse; VarParse; NParse; ParParse ] 

    let AexpParse = TermParse 

    let CParse = between (pchar ''') (pchar ''') anyChar |>> C <?> "Char"
    let CVParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CharValue"
    let ITCParse = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"
    let TUParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let TLParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    do cref := choice [CVParse; ITCParse; TUParse; TLParse; CParse]

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) : board = failwith "not implemented"

