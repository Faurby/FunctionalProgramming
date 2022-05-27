module Question4

    (* Question 4.1 *)
    
    type Colour =
        | Red | Green | Blue | Purple | Orange | Yellow
    
    type Shape =
        | Square | Circle | Star | Diamond | Club | Cross

    type tile = Shape * Colour
    
    let matchColor str =
        match str with
        | "red" -> Red
        | "green" -> Green
        | "blue" -> Blue
        | "purple" -> Purple
        | "orange" -> Orange
        | _ -> Yellow
    
    let mkTile (color: string) (shape: string) =
        match shape with
        | "square" -> (Square, matchColor color)
        | "circle" -> (Circle, matchColor color)
        | "star" -> (Star, matchColor color)
        | "diamond" -> (Diamond, matchColor color)
        | "club" -> (Club, matchColor color)
        | _ -> (Cross, matchColor color)
        
    let tileToString (t: tile) =
        let (s, c) = t
        c.ToString().ToLower() + " " + s.ToString().ToLower()
        
    (* Question 4.2 *)
    
    let sameColor (ts: tile list) =
        let (_, color) = ts.Head
        
        let rec aux (lst: tile list) =
            match lst with
            | [] -> true
            | (_, c) :: xs -> if c = color then aux xs else false
        aux ts

    let validTiles (tileLst: tile list) (t: tile) =
        let sameC = sameColor tileLst
        
        let rec aux lst =
            match lst with
            | [] -> true
            | x :: xs ->
                let (s1, c1) = x
                let (s2, c2) = t
                
                if sameC then
                    if s1 <> s2 && c1 = c2
                    then aux xs
                    else false
                else
                    if c1 <> c2 && s1 = s2
                    then aux xs
                    else false                
        aux tileLst
        
    let validTiles2 (ts: tile list) (t: tile) =
        let sameC = sameColor ts
        List.fold (fun acc elem ->
                let (s1, c1) = elem
                let (s2, c2) = t
                
                if sameC then
                    if s1 <> s2 && c1 = c2
                    then acc
                    else false
                else
                    if c1 <> c2 && s1 = s2
                    then acc
                    else false                
            ) true ts

    (* Question 4.3 optional *)

    type coord = Coord of int * int
    type board = Board of Map<coord, tile>
    type direction = Left | Right | Up | Down

    let moveCoord (Coord(x, y)) direction =
        match direction with
        | Left -> Coord (x-1, y)
        | Right -> Coord (x+1, y)
        | Up -> Coord (x, y-1)
        | Down -> Coord (x, y+1)
        
    let collectTiles (Board(b)) startCoord direction =
        
        let rec aux (c: coord) acc =
            match Map.tryFind c b with
            | None -> acc
            | Some a -> aux (moveCoord c direction) (a :: acc)
        aux startCoord List<tile>.Empty
        
    (* Question 4.4 *)

    let placeTile (c: coord, t: tile) (Board b) =
            match Map.tryFind c b with
            | Some _ -> None
            | None ->
                let upDown = collectTiles (Board b) (moveCoord c Down) Down @ collectTiles (Board b) (moveCoord c Up) Up
                let leftRight = collectTiles (Board b) (moveCoord c Left) Left @ collectTiles (Board b) (moveCoord c Right) Right
                
                if validTiles upDown t && validTiles leftRight t then
                    let newMap = b.Add(c, t)
                    Some(Board(newMap))
                else
                    None

    (* Question 4.5 *)

    (* You may use *either* railroad-oriented programming or computation expressions.
       You do not have to use both *)

    (* Railroad-oriented programming *)
    let ret = Some
    let bind f =
        function
        | None   -> None
        | Some x -> f x
    let (>>=) x f = bind f x

    (* Computation expressions *)
    type opt<'a> = 'a option
    type OptBuilderClass() =
        member t.Return (x : 'a) : opt<'a> = ret x
        member t.ReturnFrom (x : opt<'a>) = x
        member t.Bind (o : opt<'a>, f : 'a -> opt<'b>) : opt<'b> = bind f o
    let opt = new OptBuilderClass()

    let placeTiles _ = failwith "Not implemented"