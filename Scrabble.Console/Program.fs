// Learn more about F# at http://fsharp.org

open System

let time f =
    let start = DateTime.Now
    let res = f ()
    let finish = DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function
        | 0 -> []
        | x -> ($"%s{name}%d{x}", dict, bot) :: aux (x - 1)

    aux >> List.rev

[<EntryPoint>]
let main _ =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to suppress debug output

    Console.BackgroundColor <- ConsoleColor.White
    Console.ForegroundColor <- ConsoleColor.Black
    Console.Clear()


    //let board = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

    //    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

    //    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words =
        readLines (System.IO.Path.Combine(AppContext.BaseDirectory, "Dictionaries/English.txt"))

    let handSize = 7u
    let timeout = None
    let tiles = ScrabbleUtil.English.tiles 10u
    let seed = None
    let port = 13001

    let dictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        Some(Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse)
    // None

    // Uncomment this line to call your client

    let dictionary, _ = time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)

    let ourPlayers =
        [ ("De imperative banditter 1", dictionary, DIB.Scrabble.startGame)
          ("De imperative banditter 2", dictionary, DIB.Scrabble.startGame)
          ("De imperative banditter 3", dictionary, DIB.Scrabble.startGame)
          ("De imperative banditter 4", dictionary, DIB.Scrabble.startGame) ]

    // let otherPlayers = []
    let otherPlayers =
        spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 3

    let players = ourPlayers


    do ScrabbleServer.Comm.startGame board dictionary handSize timeout tiles seed port players

    ScrabbleUtil.DebugPrint.forcePrint "Server has terminated. Press Enter to exit program.\n"
    Console.ReadLine() |> ignore

    0
