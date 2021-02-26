namespace ScrabbleBotLib


open ScrabbleUtil
open DebugPrint
open MultiSet

module Helpers =
    let charNumberToPoints (ch: int) = 
        match ch with
        | 0                                             -> 0
        | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21  -> 1
        | 4 | 7                                         -> 2
        | 2 | 3 | 13 | 16                               -> 3
        | 6 | 8 | 22 | 23 | 25                          -> 4
        | 11                                            -> 5
        | 10 | 24                                       -> 8
        | 17 | 26                                       -> 10
        | _                                             -> failwith "Not valid character index"

    let charToInteger ch = 
        if (ch = '?') then 0u
        else uint32(System.Char.ToUpper(ch)) - 64u

    let uintToChar id = char(id + 64u)


    let printFunTime (func: unit -> 'a) (str: string) =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let result = func()
        stopWatch.Stop()
        debugPrint (sprintf "********** %A (TOOK: %AMS) **********\n" str (stopWatch.Elapsed.TotalMilliseconds))
        result

    module RegEx =
        open System.Text.RegularExpressions

        let (|Regex|_|) pattern input =
            let m = Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None

        let parseMove ts =
            let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
            Regex.Matches(ts, pattern) |>
            Seq.cast<Match> |> 
            Seq.map 
                (fun t -> 
                    match t.Value with
                    | Regex pattern [x; y; id; c; p] ->
                        ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                    | _ -> failwith "Failed (should never happen)") |>
            Seq.toList

    module Print =
        
        let printHand pieces (hand: MultiSet<uint32>) =
            hand |>
            MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

    module State = 
        type state = {
            playerNumber  : uint32
            hand          : MultiSet<uint32>
            board         : boardProg
            boardState    : Map<coord, (char * int)>
            numPlayers    : uint32
            words         : List<string>
            playerTurn    : uint32
        }

        let mkState pn h b num words board pTurn = { playerNumber = pn; hand = h; boardState = b; numPlayers = num; words = words; board = board; playerTurn = pTurn }
        let newState pn hand b num words board pTurn = mkState pn hand b num words board pTurn
