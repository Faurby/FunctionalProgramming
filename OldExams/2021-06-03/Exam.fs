type direction = North | East | South | West
type coord = C of int * int

// Question 1.1
let move dist dir (C(x,y))=
    match dir with
    | North -> C (x, y - dist)
    | East -> C (x + dist, y)
    | South -> C (x, y + dist)
    | West -> C (x - dist, y)

let turnRight dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft dir =
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South
    
// Question 1.2
type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (P(C(x,y), (dir:direction))) (m: move) =
    match m with
    | TurnLeft -> P (C(x,y), turnLeft dir)
    | TurnRight -> P (C(x,y), turnRight dir)
    | Forward dist -> P (move dist dir (C(x,y)), dir)
    
// Question 1.3
// recursion
let rec walk (P(C(x,y), dir)) (ms: move list) =
    match ms with
    | [] -> P(C(x,y), dir)
    | m::ms -> walk (step (P(C(x,y), dir)) m) ms
    
// higher order functions
let walk2 (P(C(x,y), dir)) (ms: move list) =
    List.fold (fun acc elem -> step acc elem) (P((C(x,y), dir))) ms

// Question 1.4
let rec path (P(C(x,y), dir)) (ms: move list) : coord list=
    match ms with
    | [] -> [C(x,y)]
    | m::ms ->
        match m with
        | Forward _ ->
            C(x,y)::path (step (P(C(x,y), dir)) m) ms
        | _ -> path (step (P(C(x,y), dir)) m) ms
        
// Question 1.5
let path2 (P(C(x,y), dir)) (ms: move list) =
    let rec aux (P(C(x,y), dir)) ms acc =
        match ms with
        | [] -> List.rev (C(x,y)::acc)
        | m::ms ->
            match m with
            | Forward _ ->
                aux (step (P(C(x,y), dir)) m) ms (C(x,y)::acc)
            | _ -> aux (step (P(C(x,y), dir)) m) ms acc
    aux (P(C(x,y), dir)) ms []
        
// Question 1.6
// Explain why path is not recursive, evaluate a function call similar to what is done in
// chapter 1.4 in HR.

// tail recursion using continuation
let path3 (P(C(x,y), dir)) (ms: move list) =
    let rec aux (P(C(x,y), dir)) ms c =
        match ms with
        | [] -> c [(C(x,y))]
        | m::ms ->
            match m with
            | Forward _ ->
                aux (step (P(C(x,y), dir)) m) ms (fun acc -> C(x,y)::acc)
            | _ -> aux (step (P(C(x,y), dir)) m) ms c
    List.rev (aux (P(C(x,y), dir)) ms (fun acc -> C(x,y)::acc)) 

// Question 2
// Consider and run the following three functions
let foo f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None ->
            m <- Map.add x (f x) m; f x
    aux

let rec bar x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
and baz = foo bar

// Question 2.1
// What are the types of functions foo , bar , and baz ?

// foo : ('a -> 'b) -> ('a -> 'b)
// bar : recursive int -> int
// baz : partial function int -> int

// What do functions foo and baz do (skip bar )? Focus on what they do rather than how they do it.

// foo takes a function as a parameter and returns a function. 
// The function it returns, takes a key as a parameter, looks up the key in a map, and returns the value if it exists.
// If it does not exists, it saves the result of the original function evaluated with this key in the map and returns the result

// bar serves as a recursive function. It takes an integer as a parameter and returns an integer. It is fibonacci.
// baz takes an integer as a parameter and returns an integer. It is the sum of the previous two fibonacci numbers.

// All functions combined serves as the fibonacci sequence, with a map attached so we can save memory and time.

// The function foo uses a mutable variable.
// What function does it serve (why is it there)?

// So that the map can be changed dynamically by the function foo.

// What would happen if you removed the mutable keyword from the line let mutable m = Map.empty ?
// Would the function foo still work? If yes, why; if no, why not?

// No it would not, since the '<-' operator is only allowed on mutable variables.

// What would be appropriate names for functions foo , bar , and baz ?

// foo : mapLookup
// bar : aux
// baz : fibonacci

// Question 2.2
// The code includes the keyword and .
// What function does this keyword serve in general (why would you use and when writing any program)?

// mutual recursion, so that the function can be called from a previous function.

// What would happen if you removed it from this particular program and replaced it with a standard let (change
// the line and baz = foo bar to let baz = foo bar )?

// The bar function would break, as it does not know of the baz function.

// Question 2.3
// The function foo generates a warning during compilation: Warning: Incomplete pattern matches on this
// expression.
// Why does this happen, and where?

// In the pattern matching of the Map.tryFind. The pattern is incomplete because we match on Some y when the map contains the key x.
// Since we have already checked if the map contains the key x, we know that the map contains the key x. If we were to only write
// 'Some y' instead of 'Some y when ...' then the pattern would not be incomplete.

// For these particular three functions will this incomplete pattern match ever cause problems for any possible
// execution of baz ? If yes, why; if no, why not.

// No, it should not, since the check is redundant. If there is Some y, then it would mean the key exists in the map.

// The function foo has two redundant computations and is hence not as efficient as it could be. What are these
// two computations and why are they redundant?

// 'Map.containsKey x m' is redundant, since we already do this computation in the 'Map.tryFind x m'
// '(f x)' and 'f x' it the same computation, this could also be optimized.

// Write a function foo2 that does exactly the same thing as foo except that it does not generate any warnings
// and is where these two redundant computations have been eliminated

let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
            let r = f x
            m <- Map.add x r m; r
    aux

// Question 2.4
let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

// baz is slower, I do not know why.

// Question 2.5
// Write an infinite sequence bazSeq : int seq such that the first element of the sequence is equal to baz 0 , the
// second to baz 1 , the third to baz 2 and so on.
// For full credit it must be close to instantanous to access large indexes of the sequence. Do not worry when the
// resulting integers overflow.

// figure out this shit

