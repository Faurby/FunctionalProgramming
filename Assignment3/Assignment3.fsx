module Programfs
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
  | PV x -> snd w.[arithEval x w s]
  | V x -> s |> Map.tryFind x |> Option.defaultValue 0
  | Add (x, y) -> (arithEval x w s) + (arithEval y w s)
  | Sub (x, y) -> (arithEval x w s) - (arithEval y w s)
  | Mul (x, y) -> (arithEval x w s) * (arithEval y w s)
  | N x -> x

// Exercise 3.4
type cExp =
  | C of char (* Character value *)
  | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
  | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
  | CV of aExp (* Character lookup at word index *)

let rec charEval (c: cExp) (w: word) (s: Map<string, int>) =
  match c with
  | C char -> char
  | ToLower exp -> System.Char.ToLower (charEval exp w s)
  | ToUpper exp -> System.Char.ToUpper (charEval exp w s)
  | CV exp -> fst w.[arithEval exp w s]


// Exercise 3.5
type bExp =
  | TT (* true *)
  | FF (* false *)
  | AEq of aExp * aExp (* numeric equality *)
  | ALt of aExp * aExp (* numeric less than *)
  | Not of bExp (* boolean not *)
  | Conj of bExp * bExp (* boolean conjunction *)
  | IsDigit of cExp (* check for digit *)
  | IsLetter of cExp (* check for letter *)
  | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let b1 = ((V "x" .+. V "y") .=. (V "y" .+. V "x"))

let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) =
  match b with
  | TT -> true
  | FF -> false
  | AEq (exp1, exp2) -> (arithEval exp1 w s) = (arithEval exp2 w s) 
  | ALt (exp1, exp2) -> (arithEval exp1 w s)  < (arithEval exp2 w s)
  | Not exp -> not (boolEval exp w s)
  | Conj (exp1, exp2) -> (boolEval exp1 w s) && (boolEval exp2 w s)
  | IsDigit exp -> System.Char.IsDigit (charEval exp w s)
  | IsLetter exp -> System.Char.IsLetter (charEval exp w s)
  | IsVowel exp -> "aeuioæøå".Contains(System.Char.ToLower (charEval exp w s))


// Exercise 3.6
let isConsonant (c: cExp) = Not (IsVowel c)

// Exercise 3.7
type stmnt =
  | Skip (* does nothing *)
  | Ass of string * aExp (* variable assignment *)
  | Seq of stmnt * stmnt (* sequential composition *)
  | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
  | While of bExp * stmnt (* while statement *)

let rec evalStmnt (stm: stmnt) (w: word) (s: Map<string, int>) =
  match stm with
  | Skip -> s
  | Ass (x, a) -> Map.add x (arithEval a w s) s
  | Seq (stm1, stm2) -> evalStmnt stm2 w (evalStmnt stm1 w s)
  | ITE (guard, stm1, stm2) -> if (boolEval guard w s) then evalStmnt stm1 w s else evalStmnt stm2 w s
  | While (guard, stm) ->
    let b = boolEval guard w s
    if b then 
      let s' = evalStmnt stm w s
      evalStmnt (While (guard, stm)) w s'
    else
      s


// evalStmnt Skip [] Map.empty;;
// evalStmnt (Ass ("x", N 5)) [] Map.empty;;
// evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty;;
// evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;
// evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;
// evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello Map.empty;;
// evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)]);;

// Exercise 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun (stm: stmnt) =  fun (w: word) pos acc ->
    let s = Map.ofList [("_pos_", pos); ("_acc_", acc)]
    let s' = evalStmnt stm w s
    s'.["_result_"]


let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers = 
  stmntToSquareFun (Seq (Ass ("_result_", V "_acc_"), 
    While (V "i" .<. WL, 
      ITE (IsDigit (CV (V "i")), 
        Seq ( 
          Ass ("_result_", V "_result_" .*. N -1),
          Ass ("i", WL)), 
        Ass ("i", V "i" .+. N 1)))))
        
let oddConsonants = 
  (Seq (Ass ("_result_", V "_acc_"), 
    While (V "i" .<. WL, 
      ITE (IsVowel (CV (V "i")), 
        Ass ("i", V "i" .+. N 1),
        Seq (Ass ("_result_", V "_result_" .*. N -1), Ass ("i", V "i" .+. N 1))))))

stmntToSquareFun oddConsonants hello 1 5
stmntToSquareFun oddConsonants [('H', 4); ('H', 4)] 1 5
stmntToSquareFun oddConsonants [('H', 4); ('H', 4); ('H', 4)] 1 5

// Exercise 3.10
type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints (lst: square list) (w: word) =
  let a =
    List.mapi (fun i x -> List.map (fun (a,b) -> (a, b w i)) x) lst
    |> List.fold (@) []
    |> List.sortBy (fun x -> fst x)
    |> List.map (fun x -> snd x)
    |> List.fold (>>) id
  a 0

let calculatePoints2 (lst: square2 list) (w: word) = calculatePoints (lst |> List.map (fun sqr2 -> sqr2 |> List.map (fun (priority, stm) -> (priority, stmntToSquareFun stm)))) w 