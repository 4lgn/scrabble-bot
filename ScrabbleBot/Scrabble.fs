module ScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO
open DebugPrint
open MultiSet
open Ass7.ImpParser

open ScrabbleBotLib
open ScrabbleBotLib.Helpers

open System


let playGame cstream pieces (st : State.state) =

    let parsedBoard = runTextParser stmParse st.board.prog
    let dictSet = printFunTime (fun () -> Set.ofList st.words) "MAKING SET DICTIONARY"
    let maskDict = printFunTime (fun () -> MaskDictionary.ofList st.words) "MAKING MASK DICTIONARY"

    let rec aux (st : State.state) (shouldPlay: bool) (shouldPlaceAtCenter: bool) =
        if (shouldPlay) then
            // Thread.Sleep(3000)
            // Print.printHand pieces (st.hand)


            let makeableWords = 
                let oldWords = st.words
                let newWords =
                    List.fold
                        (fun acc elm ->
                            let rec aux (chList: char list) (handState: MultiSet<uint32>) =
                                match chList with
                                | x::xs -> 
                                    if (MultiSet.contains (charToInteger(x)) handState) then
                                        aux xs (MultiSet.removeSingle (charToInteger(x)) handState)
                                    else
                                        false
                                | _ -> true
                            let wordPossible = aux (Seq.toList elm) st.hand
                            if (wordPossible) then elm::acc
                            else acc
                        ) [] oldWords
                debugPrint (sprintf "********** CONDENSED WORDS FROM %A TO %A (BY ONLY KEEPING THOSE WE CAN MAKE FROM HAND) **********\n" st.words.Length newWords.Length )
                newWords

            let coordsToCheck =
                Map.fold
                    (fun acc (elm: coord) elm2 ->
                        let rec goAsFarLeft ((x, y): coord) (acc: Set<coord * bool>) (count: int) =
                            if (x < -7 || count > 6 || Map.containsKey (x, y) st.boardState) then acc
                            else
                                goAsFarLeft (x-1,y) (Set.add ((x,y), true) acc) (count + 1)
                        let rec goAsFarUp ((x, y): coord) (acc: Set<coord * bool>) (count: int) =
                            if (y < -7 || count > 6 || Map.containsKey (x, y) st.boardState) then acc
                            else
                                goAsFarUp (x,y-1) (Set.add ((x,y), false) acc) (count + 1)
                        let (x,y) = elm
                        let leftStartSet =
                            let underSet = if (y+1 < 8) then goAsFarLeft (coord(x, y+1)) Set.empty 0 else Set.empty
                            let overSet = if (y - 1 > -8) then goAsFarLeft (coord(x, y-1)) Set.empty 0 else Set.empty
                            Set.union underSet overSet
                        let upStartSet =
                            let rightSet = if (x+1 < 8) then goAsFarUp (coord(x+1, y)) Set.empty 0 else Set.empty
                            let leftSet = if (x-1 > -8) then goAsFarUp (coord(x-1, y)) Set.empty 0 else Set.empty
                            Set.union rightSet leftSet
                        Set.union
                            (Set.union
                                (goAsFarLeft (fst elm - 1, snd elm) leftStartSet 0) (goAsFarUp (fst elm, snd elm - 1) upStartSet 0)
                            )
                            acc
                    )
                    Set.empty
                    st.boardState
                |> Seq.toArray
            

            let stopWatch = Diagnostics.Stopwatch.StartNew()
            let (bestMove, bestMovePoints) =
                if (shouldPlaceAtCenter) then
                    let startMoveInBotchDirections = [| for k in 0 .. 1 do yield (coord(0, 0), (k = 0)) |]
                    let allPossibleMoves = Array.Parallel.map (fun (coord, directionRight) -> MoveFinder.getBestMoveForCoord coord directionRight shouldPlaceAtCenter st maskDict dictSet parsedBoard makeableWords) startMoveInBotchDirections
                    Array.maxBy snd allPossibleMoves
                else
                    let allPossibleMoves = Array.map (fun (coord, directionRight) -> MoveFinder.getBestMoveForCoord coord directionRight shouldPlaceAtCenter st maskDict dictSet parsedBoard makeableWords) coordsToCheck
                    Array.maxBy snd allPossibleMoves
            stopWatch.Stop()
              
            debugPrint (sprintf "**************************************************************************************************************\n")
            debugPrint (sprintf "**************************************************************************************************************\n")
            debugPrint (sprintf "********** BEST MOVE %A (%A) (TOOK %A) **********\n" (bestMove) (bestMovePoints) (stopWatch.Elapsed.TotalMilliseconds))
            debugPrint (sprintf "**************************************************************************************************************\n")
            debugPrint (sprintf "**************************************************************************************************************\n")


            // Find a place where this word can be put on the board
            if (bestMove = "") then
                send cstream (SMPass)
            else
                send cstream (SMPlay(RegEx.parseMove bestMove))



        let msg = recv cstream

        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
            debugPrint (sprintf "**********  RECEIVED CMPlaySuccess **********\n%A\n*********************************" (ms, points, newPieces))
            let removed = List.fold (fun acc elm -> MultiSet.removeSingle (fst(snd(elm))) acc) st.hand ms
            let added = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) removed newPieces

            let newBoardState = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc ) st.boardState ms
            let st' = State.mkState st.playerNumber added newBoardState st.numPlayers st.words st.board st.playerTurn
            aux st' false false
        | RCM (CMPlayed (pid, ms, points)) ->
            (* Successful play by other player. Update your state *)
            debugPrint (sprintf "**********  RECEIVED CMPlayed **********\n%A\n*********************************" (pid, ms, points))
            let newBoardState = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc ) st.boardState ms
            let st' = State.mkState st.playerNumber st.hand newBoardState st.numPlayers st.words st.board st.playerTurn

            aux st' (pid % st.numPlayers + 1u = st.playerNumber) false
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            debugPrint (sprintf "**********  RECEIVED CMPlayFailed **********\n%A\n*********************************" (pid, ms))
            let newBoardState = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc ) st.boardState ms
            let st' = State.mkState st.playerNumber st.hand newBoardState st.numPlayers st.words st.board st.playerTurn

            aux st' (pid % st.numPlayers + 1u = st.playerNumber) false
        | RCM (CMPassed (pid)) ->
            aux st (pid % st.numPlayers + 1u = st.playerNumber) false
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RGPE err ->
            debugPrint (sprintf "**********  RECEIVED RGPE Error **********\n%A\n*********************************" err)
            aux st false false


    if (st.playerTurn = st.playerNumber) then
        aux st true true
    else 
        aux st false false

let startGame 
        (boardP : boardProg) 
        (alphabet : string) 
        (words : string list) 
        (numPlayers : uint32) 
        (playerNumber : uint32) 
        (playerTurn  : uint32) 
        (hand : (uint32 * uint32) list)
        (tiles : Map<uint32, tile>)
        (timeout : uint32 option) 
        (cstream : Stream) =
    debugPrint 
        (sprintf "Starting game!
                  number of players = %d
                  player id = %d
                  player turn = %d
                  hand =  %A
                  timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
              
    let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

    fun () -> playGame cstream tiles (State.newState playerNumber handSet Map.empty numPlayers words boardP playerTurn)
    