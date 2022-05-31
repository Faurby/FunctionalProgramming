open Exam

printfn "%A" <| (push 5 >>>= push 6 >>>= pop |> evalSM)
printfn "%A" <| (pop |> evalSM)
