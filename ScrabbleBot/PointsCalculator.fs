namespace ScrabbleBotLib

open ScrabbleUtil

module PointsCalculator =
    open Ass7.StateMonad
    open ScrabbleUtil

    let calculatePoints squares w =
        List.mapi (fun i x -> (List.map (fun (p, f) -> (p, f w i)) x)) squares
        |> List.fold (fun acc elem -> acc @ elem) []
        |> List.sortBy (fun x -> fst x)
        |> List.map (fun x -> snd x)
        |> List.fold (fun acc elem -> elem acc) 0

    let wordToPoints (word: (char * int * bool) list) (coordList: coord list) (parsedBoard: Ass7.stm) (squares: Map<int, squareProg>) =
        // Determine which "SquareFuns" are placed on those coords
        let getForCoord x y = getResult parsedBoard x y
        let folder elm (index, acc) =
            let (_,_,shouldPlace) = (List.rev word).[index]
            // If tile is already placed, set the tile beneath it to 0, which is equal to a singleLetterScore.
            if (not shouldPlace) then (index + 1, (elm, 0)::acc)
            else
                match getForCoord (fst elm) (snd elm) with
                | Success x -> (index + 1, (elm, x)::acc)
                | Failure _ -> failwith "Something went wrong."
        let coordToIndex = snd (List.foldBack folder coordList (0, []))


        // Map all squareFuns to the word and calculate the points for the given word        
        let parseSquareProg prog = Ass7.ImpParser.runTextParser Ass7.ImpParser.stmParse prog
        let newFolder elm acc =
            let (_, y) = elm
            let prioritizedSquareFuns = squares.[y]
            let allSquareProgs = Map.toList prioritizedSquareFuns
            let parsedProgs = List.map (fun elm -> ((fst elm), stmntToSquareFun (parseSquareProg (snd elm)))) allSquareProgs
            parsedProgs::acc
        let squaresMapList = List.foldBack newFolder coordToIndex []

        let wordWord =
            List.map
                (fun elm ->
                    let (ch, pnts, shouldPlace) = elm
                    (ch, pnts)
                ) word

        calculatePoints squaresMapList wordWord

