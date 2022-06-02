open Exam

//printfn "1113221 = %A" <| elToString ['1';'1';'1';'3';'2';'2';'1']
//printfn "1113221 = %A" <| elFromString "1113221"
//printfn "1113221 = %A" <| elToString (elFromString "1113221")

//printfn "11 = %A" <| ("1" |> elFromString |> nextElement |> elToString)
//printfn "21 = %A" <| ("1" |> elFromString |> nextElement |> nextElement |> elToString)
//printfn "312211 = %A" <| ("1" |> elFromString |> nextElement |> nextElement |> nextElement |> nextElement |> nextElement |> elToString)
//printfn "10172 = %A" <| ("11111111112222222" |> elFromString |> nextElement |> elToString)

//let testNext num seed =
//    let rec aux f y x =
//        match y with
//        | 0 -> x
//        | y -> aux f (y - 1) (f x)
//        
//    seed |> elFromString |> aux nextElement num |> elToString
//    
//testNext 1 "1" |> printfn "%A"

//printfn "%A" <| ringToList (ringFromList [1;2;3;4;5])
//printfn "%A" <| length (ringFromList [1;2;3;4;5])
//printfn "%A" <| ringToList (ringFromList [1;2;3;4;5])

//printfn "%A" <| ringToList (empty : int ring)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> push 6 |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> peek)
//printfn "%A" <| (([] : int list) |> ringFromList |> peek)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> pop )
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> ccw |> ccw |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> cw |> cw |> ringToList)

//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM smLength |> Option.get |> fst)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM (smPush 6) |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM smPop |> Option.get |> snd |> ringToList)
//printfn "%A" <| (([] : int list) |> ringFromList |> evalSM smPop)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM (smCW >>>= smCW) |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM (smCCW >>>= smCCW) |>Option.get |> snd |> ringToList)

//printfn "%A" <| ([1;2;2;4;5] |> ringFromList |> evalSM ringStep |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;2;4;5] |> ringFromList |> evalSM (ringStep >>>= ringStep) |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 0u) |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList)
//printfn "%A" <| ([1;2;3;4;5;6] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList)
