namespace ScrabbleBotLib
open System


// This is a space-inefficient but super fast data structure for querying custom masks such as ???E??S??
type MaskDictionary = Set<string> array array

module MaskDictionary =

    let ofList dictList =
        let maxLength = List.fold (fun acc (elm: string) -> if (elm.Length > acc) then elm.Length else acc) 0 dictList
        let arr: MaskDictionary =
            [|
                for _ in 1 .. 26 ->
                    [|
                        for _ in 1 .. maxLength ->
                            Set.empty<string>
                    |];
            |]

        List.iteri
            (fun i elm ->
                List.iteri
                    (fun j ch ->
                        let posArr = arr.[int(Char.ToUpper ch) - 65]
                        Array.set posArr j (Set.add elm posArr.[j])
                    )
                    (Seq.toList elm)
            )
            dictList
        arr

    let find (mask: string) (maskDict: MaskDictionary) =
        let rec aux chList index (accSet: Set<string>) =
            match chList with
            | x::xs ->
                if (x <> '?') then
                    let wordsThatMatchThisMask = maskDict.[int(Char.ToUpper x) - 65].[index]
                    if (accSet.IsEmpty) then
                        aux xs (index + 1) wordsThatMatchThisMask
                    else 
                        aux xs (index + 1) (Set.intersect (wordsThatMatchThisMask) (accSet))
                else
                    aux xs (index + 1) accSet
            | _     -> accSet

        aux (Seq.toList mask) 0 Set.empty