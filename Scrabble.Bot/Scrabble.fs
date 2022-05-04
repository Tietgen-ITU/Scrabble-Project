namespace DIB

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)
            
            if st.playerTurn = st.playerId then
                let move = 
                    match  Moves.getNextMove st pieces with
                    | Some a -> SMPlay a
                    | None -> SMPass

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerId st) move) // keep the debug lines. They are useful.
                send cstream move
                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerId st) move) // keep the debug lines. They are useful.

            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let tilesToRemove = List.map (fun (_, (tileId, _)) -> (tileId)) ms

                let st' =
                    List.fold (fun acc s -> State.addTileToHand acc s) st newPieces
                    |> State.placeLetters (Seq.ofList ms)
                    |> State.changeTurn
                    |> State.addPoints st.playerId points
                    |> State.removeTilesFromHand tilesToRemove // This state needs to be updated

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)

                let st' =
                    st
                    |> State.placeLetters (Seq.ofList ms)
                    |> State.addPoints pid points
                    |> State.changeTurn

                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMPassed _)
            | RCM (CMTimeout _)
            | RCM (CMPlayFailed _) ->
                (*The player failed to make a move.. let the next player have the turn*)
                let st' = State.changeTurn st
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerId: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerId
                playerTurn
                hand
                timeout
        )

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let players = [ for i in 0 .. int numPlayers - 1 -> true ]

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerId handSet players playerTurn)
