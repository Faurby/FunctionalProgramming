// Exercise 2.1
let downto1 n = 
  if n > 0 then [n .. -1 .. 1]
  else []

let downto2 a =
  match a with 
  | a when a > 0 -> [a .. -1 .. 1]
  | _ -> []

// Exercise 2.2
let rec removeOddIdx = function
  | []    -> []
  | [x]   -> [x]
  | x0::_::xs -> x0::(removeOddIdx xs)

// Exercise 2.3
let rec combinePair = function
  | x0::x1::xs -> (x0,x1) :: combinePair(xs)
  | _ -> []

// Exercise 2.4
type complex = {a: float; b: float}

let mkComplex x y = {a = x; b = y}

let complexToPair {a = a1; b = b1} = (a1, b1)
let (~-) {a = a1; b = b1} : complex = {a = (0.0 - a1); b = (0.0 - b1)}
let (~&) {a = a; b = b} = {a = a / (a**2 + b**2); b = (0.0 - b) / (a**2 + b**2)}
let (|+|) {a = a; b = b} {a = c; b = d} = {a = a+c; b = b+d}
let (|*|) {a = a; b = b} {a = c; b = d} = {a = a*c - b*d; b = b*c + a*d}
let (|-|) (a: complex) (b: complex) = a |+| (-b)
let (|/|) (a: complex) (b: complex) = a |*| (&b)

