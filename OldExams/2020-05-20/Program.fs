open Exam

//printfn "---- Q1.1 ---- \n"
//
//printfn "[true] = %A" <| insert true []
//printfn "[1;3;5;8;9] = %A" <| insert 5 [1; 3; 8; 9]
//printfn "['a'; 'c'; d'; 'e'] = %A" <| insert 'c' ['a'; 'd'; 'e']
//printfn "[1; 3; 5; 8; 9] = %A" <| insertionSort [5; 3; 1; 8; 9]
//printfn "['H'; 'e'; 'l'; 'l'; 'o'] = %A" <| insertionSort ['o'; 'l'; 'H'; 'e'; 'l']
//
//
//printfn "---- Q1.2 ---- \n"
//
//printfn "[true] = %A" <| insertTail true []
//printfn "[1;3;5;8;9] = %A" <| insertTail 5 [1; 3; 8; 9]
//printfn "['a'; 'c'; d'; 'e'] = %A" <| insertTail 'c' ['a'; 'd'; 'e']
//printfn "[1; 3; 5; 8; 9] = %A" <| insertionSortTail [5; 3; 1; 8; 9]
//printfn "['H'; 'e'; 'l'; 'l'; 'o'] = %A" <| insertionSortTail ['o'; 'l'; 'H'; 'e'; 'l']

//printfn "---- Q1.3 ---- \n"
//
//printfn "[1; 3; 5; 8; 9] = %A" <| insertionSort2 [5; 3; 1; 8; 9]
//printfn "['H'; 'e'; 'l'; 'l'; 'o'] = %A" <| insertionSort2 ['o'; 'l'; 'H'; 'e'; 'l']

printfn "---- Q1.4---- \n"

printfn "q bb abc lbcd = %A" <| insertBy String.length "abc" ["q"; "bb"; "lbcd"]
printfn "q bb abc lbcd = %A" <| insertionSortBy String.length ["bb"; "lbcd"; "q"; "abc"]





