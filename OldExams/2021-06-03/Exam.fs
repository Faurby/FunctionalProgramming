module Exam

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
    let path3 (P (startCoord, startDir)) moves =
       let rec aux f (P (coord,dir)) =
           function
           | [] -> f []
           | move::moves' ->
               match move with
               | TurnLeft | TurnRight -> (aux f (step (P (coord,dir)) move) moves') 
               | Forward dist ->
                   let (P (coord',dir')) = step (P (coord,dir)) (Forward dist)
                   aux (fun r -> f (coord'::r)) (P (coord',dir')) moves'
       aux (fun r -> (startCoord::r)) (P (startCoord, startDir)) moves
   
(* 2: Code Comprehension *)
    let foo f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y when Map.containsKey x m -> y
            | None   -> 
            m <- Map.add x (f x) m; f x

        aux

    let rec bar x =
      match x with 
      | 0 -> 0 
      | 1 -> 1
      | y -> baz (y - 1) + baz (y - 2)

    and baz = foo bar

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A:
        foo has type ('a -> 'b) -> 'a -> 'b when 'a : comparison
        bar has type int -> int
        baz has type int -> int
    

    Q: What do functions foo and baz do (skip bar)?
       Focus on what they do rather than how they do it.

    A:
        foo takes a function f and returns a function that given an input x returns f x.
        
        baz takes an integer n and returns the nth fibonacci number.
        
        A lot of people wrote something about the mutable variable here. This is ok, but not required
        as that is about how the function does something and from the outside you cannot tell (except for efficiency).
        You could write something like this:
        
        foo also keeps an internal cache of input-output results of f and f will only ever be run
        once for any specific input.

    The function foo uses a mutable variable.

    Q: What function does it serve (why is it there)?

    A: The mutable keyword is used to keep a mutable map from inputs to outputs of the function argument to foo
       
    Q: What would happen if you removed the mutable keyword from the line
       let mutable m = Map.empty? Would the function foo still work?
       If yes, why; if no, why not?

    A: No, it would not even compile.
    
       The <- operation in
        m <- Map.add x (f x) m
       only works on mutable variables and the program will not compile if m is not mutable.
       

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:
        foo could be called memoize (it does memoization), cache, remember, or something similar.
        bar could be called fibAux
        baz could be called fib
    
    *)
    
    (*
    
    A very common problem for this assignment was the confusion with what a function does with how it does it.
    The mutable state is squarely in how something is done not what is actually being computed, and the
    baz function in particular had a lot of creative suggestions for how it did things rather than what it did.
    All it does is compute fibonacci numbers. If I gave you baz as a black box you could not infer anything else
    from it. Your safest bet would most likely be that it was a tail recursive fibonacci function since it is fast.
    
    Keep this in mind. The distinction is important.
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: and is typically used to create mutually recursive functions (functions that call each other).
       Normally, in F#, functions can only call functions that have already been declared,
       but when two functions are dependent on each other this causes problems since one function
       has to be declared out of scope of the other. The and keyword gets around this by having
       several function be in scope of each other.


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and baz = foo bar" to "let baz = foo bar")?
       
    A: The program would not compile as baz would no longer be in the scope of bar, and bar would not
       be able to call baz.


    *)

(* Question 2.3 *) 

    (* 
    The function foo generates a warning during compilation:
    "Warning: Incomplete pattern matches on this expression.".

    Q: Why does this happen, and where? 

    A: It happens in the Some case of the pattern match
            match Map.tryFind x m with
                    | Some y when Map.containsKey x m -> y
                    | None   -> 
                    m <- Map.add x (f x) m; f x
        
       as it contains a guard (Map.containsKey x m) but no case for Some where this guard is false.

    Q: For these particular three functions will this incomplete pattern match
       ever cause problems for any possible execution of baz? If yes, why;
       if no, why not.

    A: No, it will not cause a problem as we match on Map.tryFind x m and we would never
       reach the Some case if the key x was not in the map m. This guard will always be true and is redundant.

    Q: The function foo has two redundant computations and is hence not as
       efficient as it could be. What are these two computations and why
       are they redundant?

    A:
        The first redundant computation is Map.containsKey x m. We know that this guard will always be true
        because otherwise the match of Map.tryFind x m would end up in the None case.
        
        The second redundant computation is that we compute f x two times, in stead of only one. in the line
        m <- Map.add x (f x) m; f x

    *)

    let foo2 f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y -> y
            | None   ->
                let y = f x
                m <- Map.add x y m; y

        aux

(* Question 2.4 *)

    let rec barbaz x =
        let baz = foo barbaz
        match x with 
        | 0 -> 0 
        | 1 -> 1
        | y -> baz (y - 1) + baz (y - 2)

    (*

    Q: Without explicitly timing the execution times, compare the execution
       times of baz and barbaz. One is slower than the other.
       Why? You do not have to give exact times, just spot which one is
       slower and explain why.

    A: barbaz is slower as it initializes foo from scratch at every recursive call.
       What this means is that the mutable map is reset for every recursive call which
       in effect negates its effect entirely - previous computations of the fibonacci numbers
       are no longer stored in this map but have to be recomputed. 

    *)
(* Question 2.5 *)

    let bazSeq = Seq.initInfinite baz
    
    (* Note that this only works because of the caching. Typically Seq.initInfinite is a very bad fit
       for these types of problems where an element of a sequence depends on the previous ones. *)

