module Programfs
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
type complex = float * float

let mkComplex x y = complex (x, y)

let complexToPair complex = (fst complex, snd complex)

let (~-) ((a, b):complex) = (-a, -b)
let (~&) ((a, b):complex) = (a / (a**2. + b**2.), 0.0 - b / (a**2. + b**2.))
let (|+|) ((a, b):complex) ((c, d):complex) = (a+c, b+d)
let (|*|) ((a, b):complex) ((c, d):complex) = (a*c - b*d, b*c + a*d)
let (|-|) (a: complex) (b: complex) = a |+| (-b)
let (|/|) (a: complex) (b: complex) = a |*| (&b)

// Exercise 2.5
let explode1 (s:string) = List.ofArray (s.ToCharArray())

let explode2 s =
  let rec explode (s:string) = function
    | n when String.length(s) <= n -> []
    | n -> s.[n] :: (explode s (n+1))
  explode s 0

// Exercise 2.6
let implode (a:char list) = List.fold (fun acc elem -> acc + string(elem)) "" a
let implodeRev (a:char list) = List.foldBack (fun elem acc -> acc + string(elem)) a ""

// Exercise 2.7
let toUpper (s:string) = s |> explode1 |> List.map (fun x -> System.Char.ToUpper x) |> implode

// Exercise 2.8
let rec ack = function
  | (m, n) when m = 0 -> n+1
  | (m, n) when m > 0 && n = 0 -> ack (m-1, 1)
  | (m, n) when m > 0 && n > 0 -> ack ((m-1), ack(m, n-1))

// Exercise 2.9
let time f =
  let start = System.DateTime.Now
  let res = f ()
  let finish = System.DateTime.Now
  (res, finish-start)

let timeArg1 f a = time (fun () -> f a)

// Exercise 2.10
let rec downto3 f n e = 
  if n <= 0 then e
  else downto3 f (n-1) (f n e)

let fac a = downto3 (*) a 1

let range g n = downto3 (fun x y -> (g x)::y) n []

// Exercise 2.11
type word = (char * int) list

let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]

type squareFun = word -> int -> int -> int

// Exercise 2.12
let singleLetterScore:squareFun = fun word pos acc-> (snd (word.[pos])) * 1 + acc
let doubleLetterScore:squareFun = fun word pos acc-> (snd (word.[pos])) * 2 + acc
let tripleLetterScore:squareFun = fun word pos acc-> (snd (word.[pos])) * 3 + acc

// Exercise 2.13
let doubleWordScore:squareFun = fun _ _ acc-> acc * 2
let tripleWordScore:squareFun = fun _ _ acc-> acc * 3

// Exercise 2.14
let isWovel = function
  | 'A' | 'E' | 'Y' | 'U' | 'I' | 'O' | 'Æ' | 'Ø' | 'Å' -> true
  | _ -> false

let rec countConsonants = 
  function
  | [] -> 0
  | x::xs when not (isWovel (System.Char.ToUpper (fst x))) -> 1 + countConsonants xs
  | _::xs -> countConsonants xs

let oddConsonants:squareFun = fun word _ acc -> 
  if (countConsonants word) % 2 <> 0 then acc * -1 
  else acc

