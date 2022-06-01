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

    let rec length lst =
        match lst with
        | Nil -> 0
        | Cons1 (_, b) | Cons2 (_, b) -> 1 + length b
    
(* Question 1.2 *)
    let split lst =
        let rec aux lst' acc1 acc2 =
            match lst' with
            | Nil -> (List.rev acc1, List.rev acc2)
            | Cons1 (a, b) -> aux b (a::acc1) acc2
            | Cons2 (a, b) -> aux b acc1 (a::acc2)
        aux lst [] []
    
    let length2 lst =
        let rec aux lst' acc1 acc2 =
            match lst' with
            | Nil -> acc1, acc2
            | Cons1 (_, b) -> aux b (1 + acc1) acc2
            | Cons2 (_, b) -> aux b acc1 (1 + acc2)
        aux lst 0 0

(* Question 1.3 *)


    let rec map f g lst =
        match lst with
        | Nil -> Nil
        | Cons1 (a, b) -> Cons1 (f a, map f g b)
        | Cons2 (a, b) -> Cons2 (g a, map f g b)

(* Question 1.4 *)

    let rec filter f g lst =
        match lst with
        | Nil -> Nil
        | Cons1 (a, b) when f a -> Cons1 (a, filter f g b)
        | Cons2 (a, b) when g a -> Cons2 (a, filter f g b)
        | Cons1(_, b) | Cons2 (_, b) -> filter f g b

(* Question 1.5 *)

    let rec fold f g acc lst =
        match lst with
        | Nil -> acc
        | Cons1 (a, b) -> fold f g (f acc a) b
        | Cons2 (a, b) -> fold f g (g acc a) b
        
//  or this one
//      let fold f g acc lst =
//        let rec aux lst' acc =
//            match lst' with
//            | Nil -> acc
//            | Cons1 (a, b) -> aux b (f acc a)
//            | Cons2 (a, b) -> aux b (g acc a)
//        aux lst acc

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

    A: 
        foo: 'a list -> 'a list -> 'a list
        bar: 'a list -> 'a list
    

    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: Sorts a list using mergesort
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A:
        bar: split
        foo: mergesort
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A: 
        a: left
        b: right
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: mutual recursion, function know about each other. E.g. if a function is written further down, functions earlier
       would not know about it.


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: It should work, since foo is compiled earlier in the file.

    *)

(* Question 2.3 *) 
    let foo2 xs ys =
        List.unfold (
            fun state ->
                match state with
                | [], y::ys -> Some (y, ([], ys))
                | x::xs, [] -> Some (x, (xs, []))
                | x::xs, y::ys when x < y -> Some (x, (xs, y::ys))
                | x::xs, y::ys -> Some (y, (x::xs, ys))
                | _ -> None
                ) (xs, ys)
    
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

    let fooTail xs ys =
        let rec aux xs' ys' c =
            match xs', ys' with
            | [], ys -> c ys
            | xs, [] -> c xs
            | x :: xs, y :: ys when x < y -> aux xs (y::ys) (fun r -> c (x :: r))
            | x :: xs, y :: ys -> aux (x::xs) ys (fun r -> c (y :: r))
        aux xs ys id

(* 3: Approximating square roots *)

(* Question 3.1 *)
    
    let nearestPerfectSquare x =
        let rec aux index prevDistance =
            let perfectSquare = index * index
            let currentDistance = abs (x - perfectSquare)
            if currentDistance < prevDistance
                then aux (index + 1) currentDistance
            else   
                index-1
        aux 0 1000000

    let approxSquare x n : float =
        let perfectSquare = nearestPerfectSquare x
        if n = 0
            then float perfectSquare
        else
            let rec aux (r: float) i =
                match i with
                | i when i = n -> (float) r
                | i -> aux ((((float) x / r) + r) / 2.0) (i+1)
            aux (float perfectSquare) 0

(* Question 3.2 *)

    let quadratic (a: int) (b: int) (c: int) num : (float * float) =
        let d = (b * b) - 4 * a * c
        let sqr = approxSquare d num
        let (x, y) = (((float -b + sqr) / (2.0 * float a)), (( (float -b) - sqr) / (2.0 * float a)))
        (x, y)

(* Question 3.3 *)

    let parQuadratic (eqs : (int * int * int) list) (numProcesses:int) (num:int) : (float * float) list =
        List.splitInto numProcesses eqs
            |> List.map (
                fun eqs ->
                    async {
                        return List.fold (fun acc (a,b,c) -> acc@[(quadratic a b c num)]) List.empty eqs
                    }
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.fold (fun acc r -> acc@r) []
(* Question 3.4 *)
    
    open JParsec
    open JParsec.TextParser

    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let spaces = many whitespaceChar
    
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2
    let operator = pchar '+' <|> pchar '-'

    let solveQuadratic str num =
        let str' = str + "\n"
        let parser =
            pint32 .>> pstring "x^2"
            .>*>. operator
            .>*>. pint32 .>> pstring "x"
            .>*>. operator
            .>*>. pint32
            .>*> pchar '=' .>*> pchar '0'
            .>> pstring "\n"
        let result = run parser str
        let ((((a, op1), b), op2), c) = getSuccess result
        let b' = if op1 = '-' then -b else b
        let c' = if op2 = '-' then -c else c
        quadratic a b' c' num

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = (int * int)

(* Question 4.2 *)
    
    let rec gcd x y =
        if y = 0 then x
        else gcd y (x % y)
        
    let mkRat n d =
        let x = gcd n d
        match (n / x, d / x) with
        | _, 0 -> None
        | (a', b') when a' < 0 && b' < 0 -> Some (-a', -b')
        | (a', b') when a' < 0 || b' < 0 -> Some (-a', abs b')
        | (a', b') -> Some (a', b')
        
    let ratToString ((a:int), (b:int)) =
        string a + " / " + string b
        
(* Question 4.3 *)
    
    let plus (a, b) (c, d) = mkRat ((a*d)+(b*c)) (b*d)
    let minus (a, b) (c, d) = mkRat ((a*d)-(b*c)) (b*d)
    let mult (a, b) (c, d) = mkRat (a*c) (b*d)
    let div (a, b) (c, d) = mkRat (a*d) (b*c)

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

    let smPlus rat =
        SM (fun state ->
                match plus state rat with
                | None -> None
                | Some rat -> Some((), rat)
            )

    let smMinus rat =
        SM (fun state ->
                match minus state rat with
                | None -> None
                | Some rat -> Some((), rat)
            )
        
    let smMult rat =
        SM (fun state ->
                match mult state rat with
                | None -> None
                | Some rat -> Some((), rat)
            )
        
    let smDiv rat =
        SM (fun state ->
                match div state rat with
                | None -> None
                | Some rat -> Some((), rat)
            )
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

    let rec calculate (opList:(rat * (rat -> SM<unit>)) list) : SM<unit>=
        state {
             match opList with
             | [] -> return ()
             | (rat, f)::opList ->
                 do! f rat
                 return! calculate opList
        }       