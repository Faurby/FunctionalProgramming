module Exam
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
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert e lst =
        match lst with
        | [] -> [e]
        | x :: xs when e <= x -> e::x::xs
        | x :: xs -> x :: insert e xs

    let rec insertionSort lst =
        match lst with
        | [] -> []
        | x :: xs -> insert x (insertionSort xs)
    
(* Question 1.2 *)

    let insertTail e lst =
        let rec aux lst acc =
            match lst with
            | [] -> e::acc
            | x :: xs when e < x -> (List.rev <| e::x::xs) @ acc
            | x :: xs -> aux xs (x :: acc)
        List.rev <| aux lst []
            
            
    let insertionSortTail lst =
        let rec aux lst acc =
            match lst with
            | [] -> acc
            | x :: xs -> aux xs (insertTail x acc)
        aux lst []

(* Question 1.3 *)

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    In the insert function we have two base cases, the empty list and if the
    element fits into a given spot.
    Higher order function does not support this. Higher order functions does not
    support not itterating through the whole list. We do not have a way of
    returning in the middle of a fold
    
    *)

    let insertionSort2 lst = List.fold (fun acc elem -> insertTail elem acc) [] lst

(* Question 1.4 *)

    let insertBy f e lst =
            let rec aux lst acc =
                match lst with
                | [] -> e::acc
                | x :: xs when f e < f x -> (List.rev <| e::x::xs) @ acc
                | x :: xs -> aux xs (x :: acc)
            List.rev <| aux lst []
            
    let insertionSortBy f lst =
        List.fold (fun acc elem -> insertBy f elem acc) [] lst
        

(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs
    
    let super y xs = (foo y >> baz >> bar y) xs

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
        foo: 'a -> 'a list -> 'a list
        bar: 'a -> 'a list list -> 'a list list
        baz: 'a list -> 'a list list


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A: 
        foo: takes one parameter, and removes the first occurence of it from
             the list
             e.g. foo 1 [1;2;3] = [2;3]
        
        bar: Takes one parameter, and puts it in front of all lists, inside of 'a list list
             e.g. bar 1 [[2];[3];[4]] = [[1;2];[1;3];[1;4]]
        
        baz: Takes a list as input, and "scrambles" it for finding all the
             different combinations that exists
             e.g. baz [1;2;3] = [[1;2;3];[2;3;1];[3;2;1];[1;3;2]] etc.


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:
        foo = removeSingle
        bar = prependOnLists
        baz = getCombinations
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: It happens in the "function" match case, due to it not 
       matching on an empty list.

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No, due to baz matching [] -> [], so no empty list will be sent to foo. 
       This can also be seen as it matches xs (something in the list) and 
       sends that xs to foo.

    *)

    let rec foo2 x = 
        function 
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo2 x ys)
        

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression
    
    A: 'a -> 'a list -> 'a list list

    Q: What does it do? Focus on what it does rather than how it does it.

    A: It takes an input parameter, and finds all combinations of the list, starting with
    this parameter. The list MUST contain the parameter. It removes the first occurence
    of the parameter in the list, and places the parameter at the front of the list.

    *)

(* Question 2.4 *)

    let bar2 x xs = List.map (fun y -> x::y) xs

(* Question 2.5 *)

    let rec baz2 =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> List.collect (fun y -> (foo y >> baz >> bar y) xs) xs 
    
//    or this, but it is not in correct rækkefølge
//    let rec baz2 =
//        function
//        | [] -> []
//        | [x] -> [[x]]
//        | xs  -> List.fold (fun acc elem -> (foo elem >> baz >> bar elem)xs @ acc) [] xs 

(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A: 
        foo 3 [1;2;3]
        1 :: (foo 3 [2;3]
        1 :: (2 :: (foo 3 [3]))
        1 :: (2 :: ([]))        -> recursion ends, returns back trough the call stack
        1 :: [2]
        [1;2]

    *)

    let fooTail y lst =
        let rec aux xs c =
            match xs with
            | [] -> c []
            | x' :: xs' when x' = y -> c xs'
            | x' :: xs' -> aux xs' (fun a -> c (x' :: a))
        aux lst id
        
(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape = Rock | Paper | Scissor
    type result = P1Wins | P2Wins | Draw
    
    let mkShape s =
        match s with
        | "rock" -> Rock
        | "paper" -> Paper
        | "scissors" -> Scissor
        
    let resultToString r =
        match r with
        | P1Wins -> "playerOneWin"
        | P2Wins-> "playerTwoWin"
        | Draw-> "draw"
        
    let shapeToString s =
        match s with
        | Rock -> "rock"
        | Paper -> "paper"
        | Scissor -> "scissors"

    let rps s1 s2 =
        match s1, s2 with
        | Rock, Paper | Scissor, Rock | Paper, Scissor -> P2Wins
        | Paper, Rock | Rock, Scissor | Scissor, Paper -> P1Wins
        | _, _ -> Draw

(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot s moves =
        match moves with
        | (_, s2) :: _ -> s2
        | _ -> s

    let beatingStrat moves =
        let opponentMoves = List.map snd moves
        let numRocks = opponentMoves |> List.filter (fun x -> x = Rock) |> List.length
        let numScissors = opponentMoves |> List.filter (fun x -> x = Scissor) |> List.length
        let numPapers = opponentMoves |> List.filter (fun x -> x = Paper) |> List.length
        
        if numScissors >= numPapers && numScissors >= numRocks then
            Rock
        elif numRocks >= numPapers && numRocks >= numScissors then
            Paper
        else
            Scissor

    let roundRobin lst =
        let mutable temp = lst
        let rec aux () =
            match temp with
            | [] ->
                temp <- lst
                aux ()
            | x :: xs ->
                temp <- xs
                x
        fun _ -> aux ()
        
(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: 
        Seq.initInfinite only works well when used with functions that do not require
        previous entries in the sequence to calculate its elements. If you use it in this
        case, then for the nth game, all games prior to that game must be computed, and
        then recomputed when moving on to game (n+1).
    
    *)

    let bestOutOf strat1 strat2 =
        let unfolder = (fun (p1moves, p2moves, p1, p2) ->
            let s1 = strat1 p1moves
            let s2 = strat2 p2moves
            let (p1', p2') =
                match rps s1 s2 with
                | P1Wins -> (p1 + 1, p2)
                | P2Wins -> (p1, p2 + 1)
                | Draw-> (p1, p2)
            Some ((p1', p2'), ((s1, s2) :: p1moves, (s2, s1) :: p2moves, p1', p2')))
        Seq.unfold unfolder ([], [], 0, 0) 
        |> Seq.append (Seq.singleton (0, 0))

(* Question 3.4 *)

    let playTournament rounds players =
        
        let rec initRound acc =
            function
            | [] -> (acc, [])
            | [x] -> (acc, [x])
            | x :: y :: xs -> initRound ((x, y) :: acc) xs
        
        let rec aux =
            function
            | [] -> None
            | [(_, id)] -> Some id
            | players ->
                let (pairs, rest) = initRound [] players
                
                pairs |>
                List.map
                    (fun ((p1, id1), (p2, id2)) ->
                        async {
                            let (p1win, p2win) = bestOutOf p1 p2 |> Seq.item rounds
                            return
                                if p1win = p2win
                                    then None
                                elif p1win > p2win
                                    then Some (p1, id1)
                                else
                                    Some (p2, id2)
                        }) |>
                    Async.Parallel |>
                    Async.RunSynchronously |>
                    Array.toList |>
                    List.filter Option.isSome |>
                    List.map Option.get |>
                    (fun lst -> aux (lst @ rest))
        aux (List.mapi (fun i x -> (x, i)) players)
    
    
(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = int list

    let emptyStack = List<int>.Empty

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (S f) = f emptyStack 

    let push x = S (fun s -> Some ((), x::s))
    let pop = S (fun s ->
        match s with
        | [] -> None
        | x :: xs -> Some (x, xs))

(* Question 4.3 *)

    let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: 
    
    Write: With a function like printfn, which is a function with the side effect of
    printing to the console, we want to make sure that it is run when we want it to
    run. For this reason we’re sequencing the function call with returning the
    SM<unit>.
    
    Read: Here it is again important that the order in which we read, is correct. If
    two read functions are called after each other, it is important that it is in that
    order which they read the input
    
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)
    

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()
    
    let isInt (str: string) : bool =
        System.Int32.TryParse str |> fst
    
    let binop op =
        pop >>= (fun x1 -> pop >>= (fun x2 -> op x2 x1 |> push))

    let calculateRPN () =
        let rec aux () =
            read >>= (fun s ->
                match s with
                | Some "+" -> binop ( + ) >>>= aux ()
                | Some "-" -> binop ( - ) >>>= aux ()
                | Some "*" -> binop ( * ) >>>= aux ()
                | Some str when isInt str -> push (int str) >>>= aux ()
                | Some _ -> fail
                | None -> pop >>= fun x -> string x |> write)
        aux ()