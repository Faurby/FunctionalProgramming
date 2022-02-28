open MyModules.MultiSet

// Green Exercises Multiset
printf "%A\n" (empty)
printf "%A\n" (isEmpty(empty))
printf "%A\n" (size(empty))
printf "%A\n" (contains "a" (empty))
printf "%A\n" (numItems "a" (empty))
printf "%A\n" (add "a" 3u (empty))
printf "%A\n" (addSingle "a" (empty))
printf "%A\n" (remove "a" 4u (empty))
printf "%A\n" (remove "a" 3u (add "a" 4u (empty)))
printf "%A\n" (fold (fun acc _ value -> acc + value) 0u (add "a" 4u (addSingle "b" (empty))))
printf "%A\n" (foldBack (fun _ value acc -> acc + value) (add "a" 4u (addSingle "b" (empty))) 0u)

// Yellow Exercises Multiset
printf "%A\n" (ofList ["a";"b";"c"])
printf "%A\n" (toList (add "a" 4u (addSingle "b" (empty))))
printf "%A\n" (map (fun x -> x + "O") (add "a" 4u (addSingle "b" (empty))))
