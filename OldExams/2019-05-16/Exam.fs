module exam

// Question 1.1
type Peano =
    | O
    | S of Peano
    
let toInt p =
    let rec aux p n =
        match p with
        | O -> n
        | S p' -> aux p' (n + 1u)
    aux p 0u

let fromInt (i:uint32) =
    let rec aux i =
        match i with
        | 0u -> O
        | n -> S (aux (n - 1u))
    aux i

// Question 1.2
let rec add (p1:Peano) (p2:Peano) =
    match p1 with
    | O -> p2
    | S p1' -> add p1' (S p2)
    
let rec mult (p1:Peano) (p2:Peano) =
    match p1 with
    | O -> O
    | S p1' -> add p2 (mult p1' p2)

let rec pow (a:Peano) (b:Peano) =
    match b with
    | O -> S O
    | S n -> mult a (pow a n )

// Question 1.3
let tailAdd (p1:Peano) (p2:Peano) =
    let rec aux p acc =
        match p with
        | O -> acc
        | S p' -> aux p' (S acc)
    aux p1 p2

let tailMult (p1:Peano) (p2:Peano) =
    let rec aux p acc =
        match p with
        | O -> acc
        | S p' -> aux p' (tailAdd acc p2)
    aux p1 O
    
let tailPow (a:Peano) (b:Peano) =
    let rec aux p acc =
        match p with
        | O -> acc
        | S n -> aux n (tailMult acc a)
    aux b (S O)
    
// Question 1.4
let rec loop (f: 'a -> 'a) acc (p:Peano) =
    match p with
    | O -> acc
    | S p' -> loop f (f acc) p'
    
// Question 1.5
let loopAdd (a: Peano) (b: Peano) = loop S a b
let loopMult (a: Peano) (b: Peano) = loop (fun x -> loopAdd x a) O b
let loopPow (a: Peano) (b: Peano) = if b = O then (S O) else loop (fun x -> loopMult x a) (S O) b