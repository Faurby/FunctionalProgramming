// Arithmetic Expression can either be an integer,
// or addition, subtraction, multiplication of two aExp
type aExp =
  | N of int // Integer value
  | V of string // Variable
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

// Exercise 3.1
let rec arithEvalSimple =
  function
  | N x -> x
  | Add (x,y) -> (arithEvalSimple x) + (arithEvalSimple y)
  | Sub (x,y) -> (arithEvalSimple x) - (arithEvalSimple y)
  | Mul (x,y) -> (arithEvalSimple x) * (arithEvalSimple y)
  | _ -> invalidArg "error" "invalid argument"

let validString =
  function
  | Some x -> x
  | None -> 0

let rec arithEvalState =
  function
  | N a -> a
  | Add (x,y) -> arithEvalState x
  | Sub (x, y) -> arithEvalState x
  | V a -> validString (Map.tryFind a)

let rec arithEvalState2 =
  function
  | a -> if (Map.containsKey a) then a else 0

let rec arithEvalState2 =
  function
  | a when Map.containsKey a -> a
  | _ -> 0