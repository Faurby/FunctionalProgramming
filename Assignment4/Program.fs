open MultiSet

// Green Exercises Multiset
// printfn "%A" (empty)
// printfn "%A" (isEmpty(empty))
// printfn "%A" (size(empty))
// printfn "%A" (contains "a" (empty))
// printfn "%A" (numItems "a" (empty))
// printfn "%A" (add "a" 3u (empty))
// printfn "%A" (addSingle "a" (empty))
// printfn "%A" (remove "a" 4u (empty))
// printfn "%A" (remove "a" 3u (add "a" 4u (empty)))
// printfn "%A" (fold (fun acc _ value -> acc + value) 0u (add "a" 4u (addSingle "b" (empty))))
// printfn "%A" (foldBack (fun _ value acc -> acc + value) (add "a" 4u (addSingle "b" (empty))) 0u)

// // Yellow Exercises Multiset
// printfn "%A" (ofList ["a";"b";"c"])
// printfn "%A" (toList (add "a" 4u (addSingle "b" (empty))))
// printfn "%A" (map (fun x -> x + "O") (add "a" 4u (addSingle "b" (empty))))

open Dictionary

printfn "----- INSERT Hello -----\n %A\n" (insert "Hello" (empty()))
printfn "----- INSERT e -----\n %A\n" (insert "e" (empty()))
printfn "----- LOOKUP -----\n %A\n" (lookup "Hello" (insert "Hello" (empty())))
printfn "----- STEP Some(false, dic) ----- \n %A\n" (step 'H' (insert "Hello" (empty())))
printfn "----- STEP2 Some(true, dic) ----- \n %A\n" (step 'e' (insert "e" (empty())))
printfn "----- STEP3 None ----- \n %A\n" (step 'e' (insert "Hello" (empty())))
printfn "----- STEP4 None (leaf) ----- \n %A\n" (step 'e' (empty()))