module Exam2020_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020_2 = *)

(* 1: Binary search trees *)

    type 'a bintree = 
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

    let insert _ = failwith "not implemented"
    
(* Question 1.2 *)

    let fromList _ = failwith "not implemented"

(* Question 1.3 *)

    let fold _ = failwith "not implemented"
    let foldBack _ = failwith "not implemented"
    let inOrder _ = failwith "not implemented"

(* Question 1.4 *)

    (* 

    Q: Consider the following map function

    *)

    let rec badMap f =
      function
      | Leaf -> Leaf
      | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

    (*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree` 
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: <Your answer goes here>
    *)

    let rec map _ = failwith "not implemented"

(* 2: Code Comprehension *)
    let rec foo =
        function 
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: <Your answer goes here>


    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: <Your answer goes here>


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: <Your answer goes here>

    *)

    let foo2 _ = failwith "not implemented"
    let bar2 _ = failwith "not implemented"

    (* Uncomment code to run after you have written foo2 and bar2 *)
    (*
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    *)

(* Question 2.3 *) 

    (* Consider this alternative definition of *)

    let rec foo3 =
      function 
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: <Your answer goes here>

    *)

(* Question 2.4 *)

    let bar3 _ = failwith "not implemented"

(* Question 2.5 *)

    (*

    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: <Your answer goes here>

    *)

    (* ONLY implement the one that is NOT already tail recursive *)

    let fooTail _ = failwith "not implemented"
    let bazTail _ = failwith "not implemented"

(* 3: Big Integers *)

(* Question 3.1 *)

    type bigInt = unit (* replace unit with the correct type declaration *)

    let fromString _ = failwith "not implemented"
    let toString _ = failwith "not implemented"

(* Question 3.2 *)

    let add _ = failwith "not implemented"

(* Question 3.3 *)

    let multSingle _ = failwith "not implemented"

(* Question 3.4 *)

    let mult _ = failwith "not implemented"

(* Question 3.5 *)

    let fact _ = failwith "not implemented"

(* 4: Lazy lists *)

    type 'a llist =
    | Cons of (unit -> ('a * 'a llist))

    let rec llzero = Cons (fun () -> (0, llzero))

(* Question 4.1 *)

    let step _ = failwith "not implemented"
    let cons _ = failwith "not implemented"

(* Question 4.2 *)

    let init _ = failwith "not implemented"

(* Question 4.3 *)

    let llmap _ = failwith "not implemented"

(* Question 4.4 *)

    let filter _ = failwith "not implemented"

(* Question 4.5 *)

    let takeFirst _ = failwith "not implemented"

(* Question 4.6 *)

    let unfold _ = failwith "not implemented"

    (* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

    let fib x =
        let rec aux acc1 acc2 =
            function
            | 0 -> acc1
            | x -> aux acc2 (acc1 + acc2) (x - 1)

        aux 0 1 x

    (* Uncomment after you have implemented init and unfold *)

(*
    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
  *)  
    (* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: <Your answer goes here>
    
    *)
