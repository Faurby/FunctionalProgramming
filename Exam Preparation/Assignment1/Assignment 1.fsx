// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow(x, n)

// Exercise 1.3
let rec sum n =
    match n with
    | 0 -> 0
    | n -> n + sum(n - 1)

// Exercise 1.4
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)

// Exercise 1.5
let dup (s:string) = s + s

// Exercise 1.6
let rec dupn s n =
    match n with
    | 0 -> ""
    | n -> s + dupn s (n - 1)

// Exercise 1.7
let rec bin (n, k) =
    match (n, k) with
    | (_, 0) -> 1
    | (_, _) when n = k -> 1
    | (n, k) when n <> 0 && k <> 0 && n > k -> bin(n - 1, k - 1) + bin(n - 1, k)

// ----- YELLOW -----

// Exercise 1.8
let timediff (t1h, t1m) (t2h, t2m) =
    t2h * 60 + t2m - t1h * 60 - t1m

// Exercise 1.9
let minutes (h, m) = 
    timediff (0, 0) (h, m)

// Exercise 1.10
let curry f x y = f (x,y)
let uncurry f (x, y) = f x y