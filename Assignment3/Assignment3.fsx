// Arithmetic Expression can either be an integer,
// or addition, subtraction, multiplication of two aExp

// Setup
type aExp =
  | N of int // Integer value
  | V of string // Variable
  | WL // Length of the word
  | PV of aExp // Point value of character at specific word index
  | Add of aExp * aExp // Addition
  | Sub of aExp * aExp // Subtraction
  | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42;
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

// Exercise 3.1
let rec arithEvalSimple =
  function
  | N x -> x
  | Add (x,y) -> (arithEvalSimple x) + (arithEvalSimple y)
  | Sub (x,y) -> (arithEvalSimple x) - (arithEvalSimple y)
  | Mul (x,y) -> (arithEvalSimple x) * (arithEvalSimple y)
  | _ -> invalidArg "error" "invalid argument"


// Exercise 3.2
let rec arithEvalState (a: aExp) (s: Map<string, int>) =
  match a with
  | N a -> a
  | Add (x,y) -> (arithEvalState x s) + (arithEvalState y s)
  | Sub (x, y) -> (arithEvalState x s) - (arithEvalState y s)
  | V a -> s |> Map.tryFind a |> Option.defaultValue 0 // wizardy :o

// Exercise 3.3
type word = (char * int) list
let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]


let rec arithEval (a: aExp) (w: word) (s: Map<string, int>) =
  match a with
  | WL -> w.Length
  | PV x when s.IsEmpty -> snd w.[arithEvalSimple x]
  | PV x -> arithEval x w s
  | V "_pos_" -> snd w.[s.TryFind "_pos_" |> Option.defaultValue 0]
  | V "_acc_" -> s |> Map.tryFind "_acc_" |> Option.defaultValue 0
  | Add (x, y) -> (arithEval x w s) + (arithEval y w s)
  | Sub (x, y) -> (arithEval x w s) - (arithEval y w s)
  | Mul (x, y) -> (arithEval x w s) * (arithEval y w s)
  | N x -> x

// let mylist =  [("a", 1); ("b", 2); ("c", 4)]
// Map.fold (fun acc _ value -> acc + value ) 0 mylist