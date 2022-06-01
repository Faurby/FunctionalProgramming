open System
open Exam2021_2

let testQ1 () =

    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get

    printfn "17 / 12 = %A" <| (r1 |> evalSM (smPlus r2) |> Option.get |> snd |> ratToString)
    printfn "-1 / 12 = %A" <| (r1 |> evalSM (smMinus r2) |> Option.get |> snd |> ratToString)
    printfn "1 / 2 = %A" <| (r1 |> evalSM (smMult r2) |> Option.get |> snd |> ratToString)
    printfn "8 / 9 = %A" <| (r1 |> evalSM (smDiv r2) |> Option.get |> snd |> ratToString)
    
    ()
let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    0 // return an integer exit code
