open exam

//printfn "%A\n" (fromInt 1u)
//printfn "%A\n" (fromInt 10u)
//printfn "%A\n" (toInt (fromInt 4u))
//printfn "%A\n" (toInt (add  (S (S O)) (S (S (S O)))))
//printfn "%A\n" (toInt (mult  (S (S O)) (S (S (S O)))))
//printfn "%A\n" (toInt (pow  (S (S O)) (S (S (S O)))))

//printfn "%A\n" (toInt (tailAdd  (S (S O)) (S (S (S O)))))
//printfn "%A\n" (toInt (tailMult  (S (S O)) (S (S (S O)))))
//printfn "%A\n" (toInt (tailPow  (S (S O)) (S (S (S O)))))

//printf "%A\n" (loop not true O)
//printf "%A\n" (loop not true (S (S (S O))))
//printf "%A\n" (loop not true (S (S (S (S (S (S (S (S O)))))))))

printfn "%A\n" (toInt (loopAdd  (S (S O)) (S (S (S O)))))
printfn "%A\n" (toInt (loopMult  (S (S O)) (S (S (S O)))))
printfn "%A\n" (toInt (loopPow  (S (S O)) (S (S (S O)))))
