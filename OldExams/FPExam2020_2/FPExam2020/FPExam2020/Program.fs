open System
open Exam2020_2

let testQ1() =
    let t1 = fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10])
    printfn "6 = %A" t1
            
    let t2 = foldBack (fun acc x -> x - acc) 0 (fromList [3;5;4;10])
    printfn "-6 = %A" t2
    
    let t3 = inOrder (fromList [5;3;4;10])
    printfn "[3;4;5;10] = %A" t3
    
    ()

let testQ2() =
    // place debug prints for Q2 here
    ()

let testQ3 =
//    printfn "54: %A" (add (fromString"15") (fromString"39") |>toString)
//    printfn "10003: %A" (add (fromString"9995") (fromString"8") |>toString)
//    printfn "1111111111111111110: %A" (add (fromString "123456789123456789") (fromString "987654321987654321") |> toString)
//    printfn "%A" (fromString "0")
//    printfn "32: %A" (multSingle (fromString "4") 8 |> toString)
//    printfn "0: %A" (multSingle (fromString "424") 0 |> toString)
//    printfn "1111111102111111101: %A" (multSingle (fromString "123456789123456789") 9 |> toString)

    ()

let testQ4 =
//    let (hd, tl) = step llzero
//    printfn "%A %A" hd tl
//    let (hd1, tl1) = step tl
//    printfn "%A %A" hd1 tl1
//    let (hd2, tl2) = step tl1  
//    printfn "%A %A" hd2 tl2
//    
//    let (hd, tl) = step (cons 42 llzero)
//    printfn "%A %A" hd tl
//    let (hd1, tl1) = step tl
//    printfn "%A %A" hd1 tl1
    
    ()

[<EntryPoint>]
let main argv =
    testQ1()
    0 // return an integer exit code
