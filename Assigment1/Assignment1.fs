// Exercise 1.1
let sqr i : int = i*i

// Exercise 1.2
let pow x n: float = System.Math.Pow(x, n)

// Exercise 1.3
let rec sum = function
| 0 -> 0
| n -> n + sum(n-1)

// Exercise 1.4
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib(n-1) + fib(n-2)

// Exercise 1.5
let dup s : string = s+s

// Exercise 1.6
let rec dupn s n = 
match n with
| 0 -> ""
| n -> s + dupn s (n-1)

// Exercise 1.7
let rec bin = function
| (n, 0) -> 1
| (n, m) when n = m -> 1
| (n, k) -> bin(n-1, k-1) + bin(n-1, k)

// Exercise 1.8
let timediff (a1, b1) (a2, b2) =
if a2 > a1 then b2%b1+(a2%a1*60)
else b2%b1-(a1%a2*60)

// Exercise 1.9
let minutes (h, m) = h*60+m

// Exercise 1.10

