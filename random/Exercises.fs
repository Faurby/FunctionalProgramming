// 3.1 triple
let t1 = (11, 59, "AM")
let t2 = (1, 15, "PM")

let time (a1, b1, c1) (a2, b2, c2) =
  match c1 with
  | c1 when c1 < c2 -> printfn "%A %A %s" a1 b1 c1
  | _ -> printfn "%A %A %s" a2 b2 c2

// 3.1 record
type Meridiem = AM | PM

type Time = {
  h: int;
  m: int; 
  merid: Meridiem
}

let t1' = { h = 11; m = 59; merid = AM }
let t2' = { h = 1; m = 15; merid = PM }

let comesFirst { h = h1; m = m1; merid = t1 } { h = h2; m = m2; merid = t2 } = 
  match t1 with
  | t1 when t1 < t2 -> printfn "%A %A %A" h1 m1 t1
  | _ -> printfn "%A %A %A" h2 m2 t2

// custom prefix notation
let (&.) a b = comesFirst a b
t1' &. t2';;

// Lists
let xs = [2; 3; 2]