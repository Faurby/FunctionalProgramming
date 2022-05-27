module Question1

type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

let rec length (lst: binList<'a, 'b>) =
    match lst with
    | Nil -> 0
    | Cons1 (_, b) -> 1 + length b
    | Cons2 (_, b) -> 1 + length b
