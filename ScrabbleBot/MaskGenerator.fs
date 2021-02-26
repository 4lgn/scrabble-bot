namespace ScrabbleBotLib


module MaskGenerator =
    open System
    open System.Text.RegularExpressions
    open ScrabbleUtil

    let generateMaskForCoord (coordElm: coord) (coordMapper: Map<coord, (char * int)>) (directionRight: bool) =
        let directionSwitch arg1 arg2 =
            if (directionRight) then arg1
            else arg2

        let rec backtracker curCoord =
            let prevCoord =
                directionSwitch
                    (coord((fst curCoord - 1), (snd curCoord)))
                    (coord((fst curCoord), (snd curCoord - 1)))
            let charAtPrevCoord = Map.tryFind prevCoord coordMapper
            match charAtPrevCoord with
            | Some x -> backtracker prevCoord
            | None _ -> curCoord
        let coordElm = backtracker coordElm
        let lengthToBorder = directionSwitch (Math.Abs((fst coordElm) - 7)) (Math.Abs((snd coordElm) - 7))
        let charList =
            directionSwitch
                ([ (fst coordElm) .. (fst coordElm) + lengthToBorder ])
                ([ (snd coordElm) .. (snd coordElm) + lengthToBorder ])
        let folder elm acc =
            let coordToMap = directionSwitch (coord(elm, (snd coordElm))) (coord((fst coordElm), elm))
            match Map.tryFind coordToMap coordMapper with
            | Some x -> (fst x)::acc
            | None -> '?'::acc
        let mask =
            List.foldBack folder charList []
            |> List.toArray
            |> String
        (mask, coordElm)

    let generateSubMasks mask =
        let subMaskList = Regex.Matches(mask, @"\?*\w+\?*")
        List.mapi
            (fun index elm ->
                let rec strBuilder str i =
                    if (i < index) then
                        strBuilder (str + (subMaskList.Item i).Value) (i + 1)
                    else
                        str + (subMaskList.Item i).Value

                let str = strBuilder "" 0
                if (subMaskList.Count > 1 && index <> subMaskList.Count - 1) then
                    str.[..str.Length - 2]
                else 
                    str
            )
            (Seq.toList subMaskList)