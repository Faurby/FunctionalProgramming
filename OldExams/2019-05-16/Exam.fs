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

// Question 2
let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

let rec g xs =
    function
    | []    -> xs = []
    | y::ys -> 
        match f y xs with
        | Some xs' -> g xs' ys
        | None     -> false
        
// Question 2.1
// What are the types of functions f and g
// f: 'a -> 'a list -> 'a list option when 'a: equality
// g: 'a list -> 'a list -> bool when 'a: equality

// What do functions f and g do? Focus on what they do rather than how they do it.
// f: Takes 2 arguments, an element and a list. F removes the first occurence of x in the list.
// g: Takes 2 arguments, and compares if they are equal to each other. That is, if they have the same elements, not necessary in the same order.

// What would be appropriate names for functions f and g?
// f: removeSingle
// g: equal

// Question 2.2

// The function f generates a warning during compilation: warning FS0025: Incomplete pattern matches on this expression..
// Why does this happen, and where?
// It happens in f when matching on the list. It happens due to there being two 'when' matches. Even though we can see at the
// the two when statements complete each other, F# can not see this. 
// Write a function f2 that does the same thing as f and that does not have this problem.

let rec f2 x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None


let rec fOpt x lst =
    match lst with
    | [] -> None
    | y::ys when x = y -> Some ys
    | y::ys -> Option.map (fun ys' -> y :: ys') (fOpt x ys)
    
let rec gOpt xs ys =
    match ys with
    | [] -> xs = []
    | y :: ys -> Option.map (fun xs' -> gOpt xs' ys) (fOpt y xs) |> Option.defaultValue false
    
printfn "%A" (gOpt [1; 2; 3; 4] [5; 4; 3; 2; 1])
    
// Question 2.4


(*
       g [1;2;3] [1;2]
    -> g [2;3] [2]
    -> g [3] []
    -> [3] = [] = false

*)

let fTail x xs =
    let rec aux x lst c =
        match lst with
        | [] -> None
        | y :: ys when x = y -> Some (c ys)
        | y :: ys -> aux x ys (fun r -> c (y :: r))
    aux x xs id
   
