namespace ScrabbleBotLib

open System
open System.Text.RegularExpressions

open ScrabbleUtil
open DebugPrint

open MultiSet
open Helpers
open MaskGenerator
open PointsCalculator
open Backtracker


module MoveFinder =
    let getAllPossiblePermsOfStr (str: string) =
        // Permutations snippet taken from: http://www.fssnip.net/4u/title/Very-Fast-Permutations
        let rec permutations = function
            | []      -> seq [List.empty]
            | x :: xs -> Seq.collect (insertions x) (permutations xs)
        and insertions x = function
            | []             -> [[x]]
            | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))
        // End of permutations snippet

        let getPermsOfStr str = permutations (Seq.toList str) |> Seq.toList |> List.map (List.toArray >> String)

        let folder (acc: (int * string list)) (elm: char) =
            let (i, listAcc) = acc
            let toTest = str.[0..i]
            if (toTest.Length = 1) then (i+1, listAcc)
            else (i+1, toTest::listAcc)

        snd (List.fold folder (0, []) (Seq.toList str))
        |> List.fold (fun acc elm -> getPermsOfStr elm @ acc) [] 


    let getBestMoveForCoord (initCoord: coord) (directionRight: bool) (shouldPlaceAtCenter: bool) (st: State.state) (maskDict: MaskDictionary) (dictSet: Set<string>) (parsedBoard: Ass7.stm) (makeableWords: string list) =
        // Generate mask from start of coordinate to end of board (placing ?'s at empty places)
        // This also gives a new 'initCoord' in case of backtracking
        let stopWatch = Diagnostics.Stopwatch.StartNew()
        let (generatedMask, initCoord) = generateMaskForCoord initCoord st.boardState directionRight
        stopWatch.Stop()
        let genMaskTime = (stopWatch.Elapsed.TotalMilliseconds)

        let stopWatch = Diagnostics.Stopwatch.StartNew()
        let wordsMatchingMask =
            if (Regex.Match(generatedMask, @"\w").Success) then
                let subMaskList = generateSubMasks generatedMask
                List.fold
                    (fun acc (elm: string) ->
                        let newSet = MaskDictionary.find elm maskDict
                        let newSet = Set.filter (fun (possibleWord: string) -> possibleWord.Length <= elm.Length) newSet
                        Set.union newSet acc
                    )
                    Set.empty
                    subMaskList
                |> Set.fold (fun acc elm -> if (elm.Length <= generatedMask.Length) then elm::acc else acc) []
            else
                if (shouldPlaceAtCenter) then
                    MultiSet.toList st.hand
                    |> List.map uintToChar
                    |> List.toArray
                    |> String
                    |> getAllPossiblePermsOfStr
                    |> List.filter (fun elm -> dictSet.Contains elm && elm.Length <= generatedMask.Length)
                else
                    // Not starting turn, and "??????"
                    List.filter (fun elm -> elm.Length <= generatedMask.Length) makeableWords

        stopWatch.Stop()
        let maskMatchTime = (stopWatch.Elapsed.TotalMilliseconds)


        let checkIfThereIsACharAtCoord coord =
            match Map.tryFind coord st.boardState with
            | Some (ch, _) -> (true, ch)
            | None _ -> (false, '!')
        let handContainsCharacter ch hand =
            MultiSet.contains (charToInteger ch) hand
        let handContainsWildcard hand =
            MultiSet.contains 0u hand

        let charsWithCoordsMapper index elm =
            let charPoints = charNumberToPoints (int(charToInteger elm))
            let charCoord = if (directionRight) then coord((fst initCoord + index), (snd initCoord)) else coord((fst initCoord), (snd initCoord + index))
            (elm, charPoints, charCoord)



        // Go through each of these words, and for each word, do:
        let applicableWordsAndCoordinatesOfCharactersFolder (acc: List<List<char * int * coord * bool> * List<List<char * int * coord * bool>>>) (elm: string) = 
            // Map a list to List<char * int * coord>: [(('H', 4), (1, 0)); (('A', 1), (1, 1)); ...]
            let charsWithCoords = List.mapi charsWithCoordsMapper (Seq.toList elm)
            // Go through each character of the word with respect to coordinates and do:
            let rec checkAllCharactersInWord (hand: MultiSet<uint32>) (word: (char * int * coord) list) (acc: ((char * int * coord * bool) list * (char * int * coord * bool) list list)) (wordAdjacentOrThrough: bool) : (bool * ((char * int * coord * bool) list) * ((char * int * coord * bool) list list) * bool) =
                match word with
                | x::xs ->
                    // Will exit recursion if any char fails
                    let (currentChar, currentCharPoints, currentCharCoord) = x
                    let (thereIsACharAtCoord, charAtCoord) = checkIfThereIsACharAtCoord currentCharCoord
                    // If there already is a character placed at the current coordinate, then:
                    if (thereIsACharAtCoord) then
                        // If it is equal to the current character in the word, then:
                        if (charAtCoord = currentChar) then
                            // checkCompleted, run function for next char and with its new corresponding coordinate (SKIPPING ADDING THIS TO THE LIST)
                            checkAllCharactersInWord hand xs ((currentChar, currentCharPoints, currentCharCoord, false)::fst acc, snd acc) true
                        // else (character on board differs from the character in our word):
                        else
                            // checkFailed
                            (false, [], [], false)
                    // else (empty place on the board):
                    else
                        // If you have the piece on hand, then:
                        if (handContainsCharacter currentChar hand) then
                            // checkCompleted, run function for next char, with its updated coordinate, AND with a hand state with the "placed" char removed. (ADD THIS TO A LIST)
                            let newHandState = MultiSet.removeSingle (charToInteger(currentChar)) hand
                            let (extraWordInDict, extraWord) = backtrackAndCheckExtraWord currentChar currentCharPoints currentCharCoord directionRight dictSet st.boardState
                            let newAcc =
                                if (extraWord.Length > 0) then 
                                    ((currentChar, currentCharPoints, currentCharCoord, true)::fst acc, extraWord::snd acc)
                                else
                                    ((currentChar, currentCharPoints, currentCharCoord, true)::fst acc, snd acc)
                            if (extraWordInDict) then checkAllCharactersInWord newHandState xs newAcc (wordAdjacentOrThrough || extraWord.Length > 0)
                            else
                                (false, [], [], false)
                        // else (you don't have that piece):
                        else
                            // If you have a wild card, then:
                            if (handContainsWildcard hand) then
                                // checkCompleted, add char to list, remove it (wildcard) from hand state
                                let newHandState = MultiSet.removeSingle 0u hand
                                let wildCardCharacter = (currentChar, 0, currentCharCoord, true)
                                let (extraWordInDict, extraWord) = backtrackAndCheckExtraWord currentChar 0 currentCharCoord directionRight dictSet st.boardState
                                let newAcc =
                                    if (extraWord.Length > 0) then 
                                        (wildCardCharacter::fst acc, extraWord::snd acc)
                                    else
                                        (wildCardCharacter::fst acc, snd acc)
                                if (extraWordInDict) then checkAllCharactersInWord newHandState xs newAcc (wordAdjacentOrThrough || extraWord.Length > 0)
                                else
                                    (false, [], [], false)
                            // else
                            else
                                // checkFailed
                                (false, [], [], false)
                | _     -> (true, fst acc, snd acc, wordAdjacentOrThrough)

            // If no check failed, you should get a list of the form: (given the word HAHA, where a 'H' is already present at (1,2))
            // [(('H', 4), (1, 0)); (('A', 1), (1, 1)); (('A', 1), (1, 3)); ...])
            let (wordPossible, listOfCharsAndCoords, listOfBacktrackedExtraWords, wordAdjacentOrThrough) = checkAllCharactersInWord st.hand charsWithCoords ([], []) false
            if (wordPossible && (wordAdjacentOrThrough || shouldPlaceAtCenter)) then
                (listOfCharsAndCoords, listOfBacktrackedExtraWords)::acc
            else
                acc
        // End result will be a list of the above list. I.e. of the form List<List<(char * int) * coord>>
        let stopWatch = Diagnostics.Stopwatch.StartNew()
        let applicableWordsAndCoordinatesOfCharacters = List.fold applicableWordsAndCoordinatesOfCharactersFolder [] wordsMatchingMask
        stopWatch.Stop()
        let applicableWordsTime = (stopWatch.Elapsed.TotalMilliseconds)


        // Map the above list to a string which would represent the move, and the points for that move.
        // From:   List<List<(char * int) * coord>>       ->        List<string * int>
        // Example: [("1 0 8H4 1 1 1A1 1 3 1A1", 39); ...]
        let moveAndPointsMapper (elm: (char * int * coord * bool) list * (char * int * coord * bool) list list) =
            let splitWordAndCoordsList word = 
                List.fold
                    (fun acc elm ->
                        let (ch, points, chCoord, shouldPlace) = elm
                        ((ch, points, shouldPlace)::(fst acc), (chCoord)::(snd acc))
                    )
                    ([], [])
                    word

            let (mainWord, extraWords) = elm
            let extraPoints =
                if (extraWords.Length > 0) then
                    // ExtraWords
                    // [[('A', 1, (5, -5), false); ()...]; [('A', 1, (6, -5), false); () ...]]
                    List.fold
                        (fun acc elm ->
                            let (word, coordList) = splitWordAndCoordsList elm
                            acc + wordToPoints word coordList parsedBoard st.board.squares
                        ) 0 extraWords
                else
                    0
            // Add 50 points if you play all 7 characters from your hand
            let extraPoints = if ((List.filter (fun (_, _, _, shouldPlace) -> shouldPlace) mainWord).Length = 7) then extraPoints + 50 else extraPoints

            let (word, coordList) = splitWordAndCoordsList mainWord

            let wordPoints = wordToPoints word coordList parsedBoard st.board.squares
            let moveStringFolder acc elm =
                let (ch, pnts, coord, shouldPlace) = elm
                let (x, y) = coord
                // The only tile with 0 points is a wild card.
                let charInt = if (pnts = 0) then "0" else string(charToInteger(ch))
                if (not shouldPlace) then acc
                else string x + " " + string y + " " + charInt + string ch + string pnts + " " + acc
            let moveString = List.fold moveStringFolder "" mainWord
            let wordPoints = if (moveString.Length = 0) then 0 else wordPoints
            (moveString, (wordPoints + extraPoints))
        let stopWatch = Diagnostics.Stopwatch.StartNew()
        let movesAndPoints = List.map moveAndPointsMapper applicableWordsAndCoordinatesOfCharacters
        stopWatch.Stop()
        let mappingTime = (stopWatch.Elapsed.TotalMilliseconds)



        if (maskMatchTime > 2.0) then debugPrint (sprintf "********** (%A) GENERATING MASK TOOK: %A. FOUND MASK WORDS TOOK: %A. APPLICABLE WORDS TOOK: %A. MOVES MAPPING TOOK: %A **********\n" initCoord genMaskTime maskMatchTime applicableWordsTime mappingTime)
        // If there is a move return the maximum points move, else empty
        if (movesAndPoints.Length > 0) then List.maxBy snd movesAndPoints else ("", 0)
