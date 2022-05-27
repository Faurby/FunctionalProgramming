module Question3

    (* Question 3.1 *)
        
    let foo f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y -> y
            | None ->
                m <- Map.add x (f x) m; f x
        aux

    let rec bar x : float=
        match x with
        | 0 | 1 -> 1.0
        | y -> baz (y - 1) + baz (y - 2)
    and baz = foo bar
        
    let calculateGoldenRatio (n: int) =
        (baz (n+1)) / (baz n )

    (* Question 3.2 *)

    let grSeq = Seq.unfold(fun i -> Some (calculateGoldenRatio i, i + 1)) 0

    (* Question 3.3 *)

    let goldenRectangleSeq (x: float) = Seq.unfold (fun i -> Some (x * (x * Seq.item i grSeq), i + 1)) 0
    
    let goldenTriangleSeq (b: float) =
        Seq.unfold (fun i ->
            let gRatio = Seq.item i grSeq
            let height = b * sqrt(gRatio * gRatio - (1.0/4.0))
            let result = (b * height) / 2.0
            
            Some (result, i+1)
        ) 0

    (* Question 3.4 *)

    let goldenRectangleTriangle (b: float) =
        Seq.unfold (fun i ->
                let gRatio = Seq.item i grSeq
                let height = b * sqrt(gRatio * gRatio - (1.0/4.0))
                let resultTriangle = (b * height) / 2.0
                let resultSquare = (b * (b * Seq.item i grSeq))

                Some ((resultSquare, resultTriangle), i + 1)
            ) 0