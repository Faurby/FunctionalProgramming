// Exercise 5.1
// tail recursion using accumulator
let sumA m n =
    let rec aux acc i =
        match i with
        | i when i = n -> acc + (m+i)
        | i -> aux (acc + (m + i)) (i + 1)
    aux 0 0

// Tail recursion using continuation
let sumC m n =
    let rec aux i c =
        match i with
        | i when i = n -> c (m+n)
        | i -> aux (i + 1) (fun r -> c ((m + i) + r))
    aux 0 id

// Exercise 5.2
// Tail recursion using accumulator
let lstLengthA lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | lst -> aux (acc + 1) (lst.Tail)
    aux 0 lst

// Tail recursion using continuation
let lstLengthC lst =
    let rec aux lst c =
        match lst with
        | [] -> c 0
        | lst -> aux (lst.Tail) (fun r -> c (r + 1))
    aux lst id

// Exercise 5.3
let foldback folder lst acc =
    let rec aux lst c =
        match lst with
        | [] -> c acc
        | x::xs-> aux xs (fun r -> c (folder x r))
    aux lst id

// Exercise 5.4
// Tail recursion using accumulator
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x
    
// Tail recursion using continuation
let factC x =
    let rec aux x c =
        match x with
        | 0 -> c 1
        | x -> aux (x-1) (fun r -> c (x * r))
    aux x id
    
// ----- Yellow Exercise -----

// Exercise 5.5
// Tail recursion using accumulator
let fibA x =
    let rec aux x acc1 acc2 =
        match x with
        | 0 -> 0
        | 1 | 2 -> acc1 + acc2
        | x -> aux (x - 1) (acc2) (acc1 + acc2)
    aux x 0 1
    
// Tail recursion using continuation
let fibC x =
    let rec aux x c =
        match x with
        | 0 -> c 0
        | 1 | 2 -> c 1
        | x -> aux (x - 1) (fun r -> aux (x-2) (fun r' -> c (r + r')))
    aux x id
    