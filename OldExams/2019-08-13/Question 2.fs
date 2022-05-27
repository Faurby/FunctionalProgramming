module CodeComprehension

    let f s =
        let l = String.length s
        let rec aux =
            function
            | i when i = l -> []
            | i -> s.[i] :: aux (i + 1)

        aux 0

    let g s = 
        s |> f |>
        List.filter System.Char.IsLetter |>
        List.map System.Char.ToLower |>
        fun lst -> lst = List.rev lst

    (* Question 2.1 *)

    
//    What are the types of functions f and g?

      // f: string -> char list  
      // g: string -> bool  
    
//    What do functions f and g do? Focus on what they do rather than how they do it.

      // f: Takes a string as input parameter, and outputs the corresponding char list
      // g: Takes a string as input parameter, and return true if string does not contain letter or is palindrome
    
//    What would be appropriate names for functions f and g?
    
      // f: stringToCharList
      // g: doesNotContainLetterOrPalindrome


    (* Question 2.2 *)


    let f2 (str: string) = [for i in 0 .. (String.length str)-1 -> str.[i]]
    let f2alt (str: string) = [for char in str -> char]

    (* Question 2.3 *)

    let g2 = f >> List.filter System.Char.IsLetter >> List.map System.Char.ToLower >> fun lst -> lst = List.rev lst

    (* Question 2.4 *)

//    f "Hell"
//    (aux 0)
//    'H' :: (aux 1)
//    'H' :: ('E' :: (aux 2))
//    'H' :: ('E' :: ('L' :: (aux 3)))
//    'H' :: ('E' :: ('L' :: ('L' :: (aux 4))))
//    'H' :: ('E' :: ('L' :: ('L' :: [])))
//    'H' :: ('E' :: ('L' :: ['L']))
//    'H' :: ('E' :: ['L';'L'])
//    'H' :: ['E';'L';'L']
//    ['H';'E';'L';'L']

    let fTail (str: string) =
        let l = String.length str
        let rec aux i c =
            match i with
            | i when i = l -> c []
            | i -> aux (i+1) (fun r -> c (str.[i]::r))
        aux 0 id

    (* Question 2.5 *)

    let gOpt _ = failwith "Not implemented"