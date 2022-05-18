// Exercise 2.1
let rec downto1 n =
    if n <= 0 then []
    else n :: downto1 (n - 1)

let rec downto2 n =
    match n with
    | _ when n <= 0 -> []
    | n -> n :: downto2 (n - 1)

// Exercise 2.2
let rec removeOddIdx xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | x :: _ :: xs ->  x :: (removeOddIdx xs)

// Exercise 2.3
let rec combinePair xs =
    match xs with
    | [] -> []
    | [_] -> []
    | x :: y :: xs -> (x, y) :: (combinePair xs)

// Exercise 2.4
type complex = float * float
let mkComplex x y = complex (x, y)
let complexToPair complex = (fst complex, snd complex)

let (~-) ((a, b):complex) = (-a, -b)
let (~&) ((a, b):complex) = (a / (a**2. + b**2.), 0.0 - b / (a**2. + b**2.))


let (|+|) ((a,b):complex) ((c,d):complex) = (a+c, b+d)
let (|*|) ((a,b):complex) ((c,d):complex) = (a*c - b*d, b*c + a*d)
let (|-|) (a: complex) (b: complex) = a |+| (-b)
let (|/|) (a: complex) (b: complex) = a |*| (&b)

// Exercise 2.5
let explode1 (s:string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s:string) =
    match s with
    | ""-> []
    | s -> s.[0]::explode2 (s.[1..])

// Exercise 2.6
let implode (xs:char list) = 
    List.foldBack (fun (x:char) (acc:string) -> (string x) + acc) xs ""

let implodeRev (xs:char list) = 
    List.fold (fun (acc:string) (x:char) -> (string x) + acc) "" xs

// Exercise 2.7
let toUpper s = s |> explode1 |> List.map (fun (c:char) -> System.Char.ToUpper(c)) |> implode
let toUpper2 = explode1 >> List.map (fun (c:char) -> System.Char.ToUpper(c)) >> implode

// Exercise 2.8
let rec ack (m, n) =
    match m, n with
    | 0, _ -> n + 1
    | m, n when m > 0 && n = 0 -> ack (m - 1, 1)
    | m, n when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
    | _, _ -> 0

// ----- Yellow -----
// Exercise 2.9
let time f = 
    let start = System.DateTime.Now
    let result = f ()
    let finish = System.DateTime.Now
    (result, finish - start)

let timeArg1 f a = time (fun () -> f a)

// Exercise 2.10
let rec downto3 f n e = 
    match n with
    | _ when n <= 0 -> e
    | n -> downto3 f (n - 1) (f n e)

let fac a = downto3 (*) a 1
let range g n = downto3 (fun x acc -> (g x)::acc) n []