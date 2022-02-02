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
let timediff (a1, b1) (a2, b2) = (a2*60+b2) - (a1*60+b1)

// Exercise 1.9
let minutes (h, m) = h*60+m

// Exercise 1.10
let curry fn a b = fn(a, b)
let uncurry fn (a, b) = fn a b
uncurry (fun x y -> x, y*2) (1, 1)

// Exercise 1.11
let empty (letter, pointValue) = fun pos -> (letter, pointValue)
let theLetterA : int -> char * int = empty ('A', 1);;

// Exercise 1.12
let add newPos (letter, pointValue) word = fun pos -> if pos = newPos then (letter, pointValue) else word pos
let theLettersAB = add 1 ('B', 3) theLetterA
theLettersAB 1

// Exercise 1.13
let hello: int -> char * int = empty(char 0, 0) |> add 0 ('H', 4) |> add 1 ('E', 1) |> add 2 ('L', 1) |> add 3 ('L', 1) |> add 4 ('O', 1)

// Exercise 1.14
let singleLetterScore word = fun pos -> uncurry (fun x y -> y) (word pos)
let doubleLetterScore word = fun pos -> uncurry (fun x y -> y*2) (word pos)
let tripleLetterScore word = fun pos -> uncurry (fun x y -> y*3) (word pos)


// string to word in scrabble
let rec strwordrec input index word =
 if String.length(input) > index then
    strwordrec input (index+1) (add index (input.Chars(index), 1) word)
 else word

let strword input = strwordrec input 0 (empty(char 0, 0))

let rec wordstrrec word index (str: string) = 
 if uncurry (fun x y -> y) (word index) <> 0 then
    let char = (uncurry(fun x y -> x) (word index))
    wordstrrec word (index+1) (str + string char)
 else str

let wordstr input = wordstrrec input 0 ""

