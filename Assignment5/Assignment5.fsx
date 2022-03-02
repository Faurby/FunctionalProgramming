// Exercise 5.1
let sum a b =
    let rec aux (acc : int) = 
        function
        | x when x >= b -> acc + b
        | x -> aux (acc + x) (x+1)
    aux a 0
sum 0 10

// Exercise 5.2
let length (a: 'a list) =
    let rec aux (acc : int) =
        function
        | [] -> acc
        | x::xs -> aux (acc+1) (xs)
    aux 0 a

// Exercise 5.3
let foldBack folder lst acc =
    let rec aux lst c =
        match lst with
        | [] -> c acc
        | x::xs -> aux xs (fun r -> c (folder x r))
    aux lst id

// Exercise 5.4
let factC a =
    let rec aux x c =
        match x with
        | 0 -> c 1
        | x -> aux (x-1) (fun r -> c(x*r))
    aux a id

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

// FactA is a lot faster because it works with numbers. It uses 0
// garbage collection, and runs very fast (2ms when given 1000000).
// FactC is slower because it 1) builds one big function, containing
// all the calculations and 2) has to calculate them. It has to
// Garbage collect meanwhile this is happening. It takes
// 62ms when given 1000000

// Exercise 5.5
#time
let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA a =
    let rec aux x (acc1: int) (acc2: int) =
        match x with
        | 0 -> 0
        | 1 | 2 -> acc1+acc2
        | x -> aux (x-1) acc2 (acc1 + acc2)
    aux a 0 1

let fibC a =
    let rec aux x c =
        match x with
        | 0 -> c 0
        | 1 -> c 1
        | x -> aux (x-1) (fun r1 -> aux (x-2) (fun r2 -> c (r1 + r2)))
    aux a id

fibW 100000000
fibA 100000000
fibC 40

// fibW 100000000 took 44ms, GC 0 times.
// fibA 100000000 took 127ms, GC 0 times.
// fibC 40 took 7.3 seconds, GC 2534 times.
// It seems fibC was worst (closest to naive recursive) while
// fibW was best actually. I don't really know why fibW is best,
// while being discouraged so much "this is a truly horrible idea"...