module MultiSet

  type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32> 

  // Green exercises (multiset)
  let empty = MS (Map.empty)
  let isEmpty (MS(s)) = Map.isEmpty s 
  let size (MS(s)) = Map.fold (fun acc _ value -> acc + value) 0u s
  let contains key (MS(s)) = Map.containsKey key s
  let numItems key (MS(s)) = Map.tryFind key s |> Option.defaultValue (uint32 0)
  let add key n (MS(s)) = MS(s.Add (key, ((numItems key (MS(s))) + n)))
  let addSingle key (MS(s)) = add key 1u (MS(s))
  let remove key n (MS(s)) = 
    let occurences = numItems key (MS(s))
    if (occurences > n) then MS(s.Remove(key).Add(key, (occurences - n)))
    else MS(s.Remove key)
  let removeSingle key (MS(s)) = remove key 1u (MS(s))
  let fold f acc (MS(s)) = Map.fold f acc s
  let foldBack f (MS(s)) acc = Map.foldBack f s acc

  // Yellow exercises (multiset)
  let ofList lst = List.fold (fun acc elem -> addSingle elem acc) empty lst
  let toList s = fold (fun acc elem num -> List.init (int32 num) (fun _ -> elem) @ acc) [] s
  let map f s = ofList (List.map f (toList s))
  let union (MS(s1)) (MS(s2)) = fold (fun acc elem num -> if (contains elem s2) then  ) s2 s1