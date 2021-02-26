namespace ScrabbleBotLib

open System
open ScrabbleUtil
open Helpers

module Backtracker =
    let backtrackAndCheckExtraWord (ch: char) (chPnts: int) (chCoord: coord) (directionRight: bool) (dictSet: Set<string>) (coordsMapper: Map<coord, (char * int)>) =
        let rec backtrack (curCoord: coord) (acc: string) (directionSwitch: int) =
            let prevCoord = 
                if (directionRight) then
                    if (directionSwitch = 0) then
                        // Go up
                        coord((fst curCoord), (snd curCoord - 1))
                    else
                        // Go down
                        coord((fst curCoord), (snd curCoord + 1))
                else
                    if (directionSwitch = 0) then
                        // Go right
                        coord((fst curCoord + 1), (snd curCoord))
                    else
                        // Go left
                        coord((fst curCoord - 1), (snd curCoord))

            let charAtPrevCoord = Map.tryFind prevCoord coordsMapper
            match charAtPrevCoord with
            | Some (ch, pnts) -> backtrack prevCoord (string ch + acc) directionSwitch
            | None _ -> (curCoord, acc)


        let determineCoordsPassedThrough (coord1: coord) (coord2: coord) : coord list =
            let (x1, y1) = coord1
            let (x2, y2) = coord2
            if (x1 <> x2) then
                // word is going right
                if (x1 < x2) then [ for i in x1 .. x2 -> coord(i, y1) ]
                else [ for i in x2 .. x1 -> coord(i, y1) ]
            else
                // word is going down
                if (y1 < y2) then [ for i in y1 .. y2 -> coord(x1, i) ]
                else [ for i in y2 .. y1 -> coord(x1, i) ]

        // Simple string reverse snippet taken from https://stackoverflow.com/questions/4556160/is-there-more-simple-or-beautiful-way-to-reverse-a-string
        let rev str =
            let si = Globalization.StringInfo(str)
            let teArr = Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements(i,1))
            Array.Reverse(teArr)
            String.Join("", teArr)
        // End of string reverse snippet

        let findBacktrack : (string * coord list) =
            let (coord1, backtrackedWord1) = backtrack chCoord "" 0
            let (coord2, backtrackedWord2) = backtrack chCoord "" 1
            // Obviously no word if coord1 = coord2
            if (coord1 = coord2) then ("", [])
            else
                if (directionRight) then
                    let y1 = snd coord1
                    let y2 = snd coord2
                    if (y1 < y2) then
                        (backtrackedWord1 + string ch + (backtrackedWord2 |> rev), determineCoordsPassedThrough coord1 coord2)
                    else
                        (backtrackedWord2 + string ch + (backtrackedWord1 |> rev), determineCoordsPassedThrough coord1 coord2)
                else
                    let x1 = fst coord1
                    let x2 = fst coord2
                    if (x1 < x2) then
                        (backtrackedWord1 + string ch + (backtrackedWord2 |> rev), determineCoordsPassedThrough coord1 coord2)
                    else
                        (backtrackedWord2 + string ch + (backtrackedWord1 |> rev), determineCoordsPassedThrough coord1 coord2)


        // Check if there is a char placed adjacent (only backtrack in this case)
        let (backtrackedWord, coordListThroughWord) =
            if (directionRight) then
                match Map.tryFind (coord((fst chCoord), (snd chCoord + 1))) coordsMapper with
                | Some _ -> findBacktrack
                | None _ ->
                    match Map.tryFind (coord((fst chCoord), (snd chCoord - 1))) coordsMapper with
                    | Some _ -> findBacktrack
                    | None _ -> ("", [])
            else
                match Map.tryFind (coord((fst chCoord + 1), (snd chCoord))) coordsMapper with
                | Some _ -> findBacktrack
                | None _ ->
                    match Map.tryFind (coord((fst chCoord - 1), (snd chCoord))) coordsMapper with
                    | Some _ -> findBacktrack
                    | None _ -> ("", [])


        if (backtrackedWord = "") then (true, [])
        else 
            let result = dictSet.Contains backtrackedWord
            if (result) then
                // Should place should be true if the character is at the coord we are checking (because we want to calculate extra points for tiles beneath this.)
                let shouldPlace i = coordListThroughWord.[i] = chCoord
                let pointsForGivenChar ch i =
                    if (coordListThroughWord.[i] = chCoord) then
                        chPnts
                    else 
                        charNumberToPoints(int(charToInteger(ch)))
                let wordInOurFormat = List.mapi (fun i elm -> (elm, (pointsForGivenChar elm i), coordListThroughWord.[i], (shouldPlace i))) (Seq.toList backtrackedWord)
                (result, wordInOurFormat)
            else
                (false, [])

