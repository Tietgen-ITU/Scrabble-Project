namespace DIB

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint $"%d{x} -> (%A{Map.find x pieces}, %d{i})\n") ()

module Scrabble =
    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            if st.playerTurn = st.playerId then
                let move =
                    let mv = Moves.getNextMove st pieces

                    match mv with
                    | Some a -> SMPlay a
                    | None -> SMPass

                debugPrint $"Player %d{State.playerId st} -> Server:\n%A{move}\n" // keep the debug lines. They are useful.
                send cstream move
                debugPrint $"Player %d{State.playerId st} <- Server:\n%A{move}\n" // keep the debug lines. They are useful.

            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let tilesToRemove = List.map (fun (_, (tileId, _)) -> tileId) ms

                let st' =
                    List.fold State.addTileToHand st newPieces
                    |> State.placeLetters (Seq.ofList ms)
                    |> CrossCheck.update ms
                    |> State.changeTurn
                    |> State.addPoints st.playerId points
                    |> State.removeTilesFromHand tilesToRemove // This state needs to be updated

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)

                let st' =
                    st
                    |> State.placeLetters (Seq.ofList ms)
                    |> CrossCheck.update ms
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
            | RCM (CMForfeit pid) ->
                let st' = State.removePlayer st (int pid)
                aux st'
            | RCM a -> failwith $"not implmented: %A{a}"
            | RGPE err ->
                printfn $"Gameplay Error:\n%A{err}"
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
        debugPrint
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerId}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let players = [ for _ in 0 .. int numPlayers - 1 -> true ]

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerId handSet players playerTurn)
