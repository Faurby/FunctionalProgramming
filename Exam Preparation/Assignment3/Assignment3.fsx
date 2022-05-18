type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)

let rec arithEvalSimple aExp =
    match aExp with
    | N n -> n
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b
    | Div (a, b) -> arithEvalSimple a / arithEvalSimple b
    
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z")

let rec arithEvalState aExp (s: Map<string, int>) =
    match aExp with
    | N n -> n
    | Add (a, b) -> (arithEvalState a s) + (arithEvalState b s)
    | Sub (a, b) -> (arithEvalState a s) - (arithEvalState b s)
    | Mul (a, b) -> (arithEvalState a s) * (arithEvalState b s)
    | Div (a, b) -> (arithEvalState a s) / (arithEvalState b s)
    | V v -> Map.tryFind v s |> Option.defaultValue 0

type word = (char * int ) list
let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let rec arithEval aExp (w:word) (s: Map<string, int>) =
    match aExp with
    | N n -> n
    | Add (a, b) -> (arithEval a w s) + (arithEval b w s)
    | Sub (a, b) -> (arithEval a w s) - (arithEval b w s)
    | Mul (a, b) -> (arithEval a w s) * (arithEval b w s)
    | Div (a, b) -> (arithEval a w s) / (arithEval b w s)
    | V v -> Map.tryFind v s |> Option.defaultValue 0
    | WL -> List.length w
    | PV a -> snd w[arithEval a w s]
    
type cExp =
    | C of char
    | ToUpper of cExp
    | ToLower of cExp
    | CV of aExp
    
let rec charEval cExp (w:word) (s: Map<string, int>) =
    match cExp with
    | C c -> c
    | ToUpper a -> System.Char.ToUpper (charEval a w s)
    | ToLower a -> System.Char.ToLower (charEval a w s)
    | CV a -> fst w.[arithEval a w s]

type bExp =
    | TT
    | FF
    | AEq of aExp * aExp
    | ALt of aExp * aExp
    | Not of bExp
    | Conj of bExp * bExp
    | IsDigit of cExp
    | IsLetter of cExp
    | IsVowel of cExp

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel (c:char) =
    match c with
    | _ when "aeiouAEIOU".Contains c -> true
    | _ -> false

let rec boolEval bExp (w:word) (s: Map<string, int>) =
    match bExp with
    | TT -> true
    | FF -> false
    | AEq (a, b) -> (arithEval a w s) = (arithEval b w s)
    | ALt (a, b) -> (arithEval a w s) < (arithEval b w s)
    | Not b -> not (boolEval b w s)
    | Conj (b1, b2) -> (boolEval b1 w s) && (boolEval b2 w s)
    | IsDigit c -> System.Char.IsDigit (charEval c w s)
    | IsLetter c -> System.Char.IsLetter (charEval c w s)
    | IsVowel c -> isVowel (charEval c w s)

let isConsonant cExp = Not (IsVowel cExp)

type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

