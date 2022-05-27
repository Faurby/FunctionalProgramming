module Question1

    type Sum<'A, 'B> =
    | Left of 'A
    | Right of 'B

    (* Question 1.1 *)

    let sum1: Sum<int list, bool option> = Left [0;1;2;3;4]
    let sum2: Sum<int list, bool option> = Right (Some true)

    let sumMap f g sum =
        match sum with
        | Left x -> f x
        | Right y -> g y

    (* Question 1.2 *)

    type SumColl<'A, 'B> =
    | Nil
    | CLeft of 'A * SumColl<'A, 'B>
    | CRight of 'B * SumColl<'A, 'B>

    let sumColl: SumColl<bool list, int> = CLeft ([true;false;true], CRight (2, Nil)) 

    let rec ofList (sum: Sum<'A, 'B> list): SumColl<'a, 'b> =
        match sum with
        | head :: tail ->
            match head with
            | Left x -> CLeft (x, (ofList tail))
            | Right y -> CRight (y, (ofList tail))
        | [] -> Nil

    (* Question 1.3 *)

    let reverse (sum: SumColl<'a, 'b>): SumColl<'a, 'b> =
        let rec aux (s: SumColl<'a, 'b>) acc =
            match s with
            | Nil -> acc
            | CLeft (a, b) -> aux b (CLeft (a, acc))
            | CRight (a, b) ->  aux b (CRight (a, acc))
        aux sum Nil

    (* Question 1.4 *)

    let ofList2 (sum: Sum<'A, 'B> list) =
        List.foldBack (fun (elem: Sum<'A, 'B>) (acc: SumColl<'a, 'b>) ->
            match elem with
            | Left x -> CLeft (x, acc)
            | Right y -> CRight (y, acc)
            ) sum Nil

    (* Question 1.5 *)

    let rec foldBackSumColl f g sum acc =
        match sum with
        | CLeft (a, b) -> f a (foldBackSumColl f g b acc)
        | CRight (a, b) -> g a (foldBackSumColl f g b acc)
        | Nil -> acc
