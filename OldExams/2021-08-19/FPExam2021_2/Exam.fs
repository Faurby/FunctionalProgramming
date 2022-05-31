module Exam2021_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 = 
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

    type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

    let length _ = failwith "not implemented"
    
(* Question 1.2 *)
    let split _ = failwith "not implemented"
    let length2 _ = failwith "not implemented"

(* Question 1.3 *)


    let map _ = failwith "not implemented"

(* Question 1.4 *)

    let filter _ = failwith "not implemented"

(* Question 1.5 *)

    let fold _ = failwith "not implemented"

(* 2: Code Comprehension *)
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

    and bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: <Your answer goes here>


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: <Your answer goes here>
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: <Your answer goes here>
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A: <Your answer goes here>
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: <Your answer goes here>


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: <Your answer goes here>

    *)

(* Question 2.3 *) 
    let foo2 _ = failwith "not implemented"
    
    (* use the following code as a starting template
    let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let fooTail _ = failwith "not implemented"

(* Question 2.5 *)

    let barTail _ = failwith "not implemented"

(* 3: Approximating square roots *)

(* Question 3.1 *)

    let approxSquare _ = failwith "not implemented"

(* Question 3.2 *)

    let quadratic _ = failwith "not implemented"

(* Question 3.3 *)

    let parQuadratic _ = failwith "not implemented"

(* Question 3.4 *)

    let solveQuadratic _ = failwith "not implemented"

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = unit (* replace this entire type with your own *)

(* Question 4.2 *)

    let mkRat _ = failwith "not implemented"
    let ratToString _ = failwith "not implemented"

(* Question 4.3 *)

    let plus _ = failwith "not implemented"
    let minus _ = failwith "not implemented"
    let mult _ = failwith "not implemented"
    let div _ = failwith "not implemented"

(* Question 4.4 *)

    type SM<'a> = SM of (rat -> ('a * rat) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
                let (SM g) = f x
                g st')
        
    let (>>=) m f = bind m f
    let (>>>=) m n = m >>= (fun () -> n)
    let evalSM (SM f) s = f s 

    let smPlus _ = failwith "not implemented"
    let smMinus _ = failwith "not implemented"
    let smMult _ = failwith "not implemented"
    let smDiv _ = failwith "not implemented"

(* Question 4.5 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculate _ = failwith "not implemented"