namespace MultiSet

type MultiSet<'a when 'a : comparison> =
    {map: Map<'a, uint32>}
    override m.ToString() =
        let tempString = "{" + (Map.fold (fun acc elm n -> acc + "(" + elm.ToString() + ", #" + n.ToString() + "), ") "" m.map)
        tempString.[0..tempString.Length-3] + "}"

module MultiSet =

    let empty : MultiSet<'a> = { map = Map.empty }
    let isEmpty (m: MultiSet<'a>) : bool = Map.isEmpty m.map
    let size (m: MultiSet<'a>) : uint32 =
        Map.fold (fun acc elm n -> acc + n) 0u m.map
    let contains (v: 'a) (m: MultiSet<'a>) : bool = Map.containsKey v m.map
    let numItems (v: 'a) (m: MultiSet<'a>) : uint32 =
        if (Map.containsKey v m.map) then m.map.[v]
        else 0u
    let add (v: 'a) (n: uint32) (m: MultiSet<'a>) =
        if (Map.containsKey v m.map) then { map = Map.add v (m.map.[v]+n) m.map }
        else { map = Map.add v n m.map }
    let addSingle (v: 'a) (m : MultiSet<'a>) : MultiSet<'a> = add v 1u m
    let remove (v: 'a) (n: uint32) (m: MultiSet<'a>) : MultiSet<'a> =
        if (Map.containsKey v m.map && m.map.[v] > 1u && n < m.map.[v]) then
            { map = Map.add v (m.map.[v]-n) m.map }
        else
            if (Map.containsKey v m.map) then { map = Map.remove v m.map } else m
    let removeSingle (v: 'a) (m: MultiSet<'a>) : MultiSet<'a> = remove v 1u m
    let fold (f: ('a -> 'b -> uint32 -> 'a)) (acc: 'a) (m: MultiSet<'b>) : 'a =
        Map.fold f acc m.map
    let foldBack (f: ('a -> uint32 -> 'b -> 'b)) (m: MultiSet<'a>) (acc: 'b) : 'b =
        Map.foldBack f m.map acc
    let map (f: ('a -> 'b)) (m: MultiSet<'a>) : MultiSet<'b> =
        Map.toList m.map |> List.map (fun x -> (f (fst x), snd x)) |> List.fold (fun acc elm -> add (fst elm) (snd elm) acc) empty
    let ofList (v: 'a list) : MultiSet<'a> =
        List.fold (fun acc elm -> addSingle elm acc) empty v
    let rec lol (acc: 'a list) (elm: 'a) (n: uint32) : 'a list = 
        match n with
        | n when n > 0u -> lol (elm::acc) elm (n-1u)
        | _ -> acc
    let toList (m: MultiSet<'a>) : 'a list =
        List.foldBack (fun elm acc -> lol acc (fst elm) (snd elm)) (Map.toList m.map) []
    let union (m1: MultiSet<'a>) (m2: MultiSet<'a>) : MultiSet<'a> =
        let newMap =
            Map.fold
                (fun acc elm n ->
                    if contains elm m2 then
                        if n > m2.map.[elm] then
                            add elm n acc
                        else
                            add elm m2.map.[elm] acc
                    else
                        add elm n acc
                )
                empty
                m1.map
        let newMap2 =
            Map.fold
                (fun acc elm n ->
                    if contains elm newMap then
                        acc
                    else
                        add elm n acc
                )
                newMap
                m2.map
        newMap2
    let sum (m1: MultiSet<'a>) (m2: MultiSet<'a>) : MultiSet<'a> =
        Map.fold (fun acc elm n -> add elm n acc) m1 m2.map
    let subtract (m1: MultiSet<'a>) (m2: MultiSet<'a>) : MultiSet<'a> =
        Map.fold (fun acc elm n -> remove elm n acc) m1 m2.map
    let intersection (m1: MultiSet<'a>) (m2: MultiSet<'a>) : MultiSet<'a> =
        Map.fold (fun acc elm n -> if contains elm m1 then add elm n acc else acc) empty m2.map