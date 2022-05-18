type direction = North | East | South | West
type coord = C of int * int

// Question 1.1
let move dist dir (C(x,y))=
    match dir with
    | North -> C (x, y - dist)
    | East -> C (x + dist, y)
    | South -> C (x, y + dist)
    | West -> C (x - dist, y)

let turnRight dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft dir =
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South
    
// Question 1.2
type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (P(C(x,y), (dir:direction))) (m: move) =
    match m with
    | TurnLeft -> P (C(x,y), turnLeft dir)
    | TurnRight -> P (C(x,y), turnRight dir)
    | Forward dist -> P (move dist dir (C(x,y)), dir)
    
// Question 1.3
// recursion
let rec walk (P(C(x,y), dir)) (ms: move list) =
    match ms with
    | [] -> P(C(x,y), dir)
    | m::ms -> walk (step (P(C(x,y), dir)) m) ms
    
// higher order functions
let walk2 (P(C(x,y), dir)) (ms: move list) =
    List.fold (fun acc elem -> step acc elem) (P((C(x,y), dir))) ms

// Question 1.4
let rec path (P(C(x,y), dir)) (ms: move list) : coord list=
    match ms with
    | [] -> [C(x,y)]
    | m::ms ->
        match m with
        | Forward _ ->
            C(x,y)::path (step (P(C(x,y), dir)) m) ms
        | _ -> path (step (P(C(x,y), dir)) m) ms
        
// Question 1.5
let path2 (P(C(x,y), dir)) (ms: move list) =
    let rec aux (P(C(x,y), dir)) ms acc =
        match ms with
        | [] -> List.rev (C(x,y)::acc)
        | m::ms ->
            match m with
            | Forward _ ->
                aux (step (P(C(x,y), dir)) m) ms (C(x,y)::acc)
            | _ -> aux (step (P(C(x,y), dir)) m) ms acc
    aux (P(C(x,y), dir)) ms []
        
// Question 1.6
// Explain why path is not recursive, evaluate a function call similar to what is done in
// chapter 1.4 in HR.

// tail recursion using continuation
let path3 (P(C(x,y), dir)) (ms: move list) =
    let rec aux (P(C(x,y), dir)) ms c =
        match ms with
        | [] -> c [(C(x,y))]
        | m::ms ->
            match m with
            | Forward _ ->
                aux (step (P(C(x,y), dir)) m) ms (fun acc -> C(x,y)::acc)
            | _ -> aux (step (P(C(x,y), dir)) m) ms c
    List.rev (aux (P(C(x,y), dir)) ms (fun acc -> C(x,y)::acc)) 
