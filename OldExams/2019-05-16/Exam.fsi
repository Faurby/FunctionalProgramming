module exam

type Peano =
    | O
    | S of Peano

val toInt : Peano -> uint32
val fromInt : uint32 -> Peano

val add : Peano -> Peano -> Peano
val mult : Peano -> Peano -> Peano
val pow : Peano -> Peano -> Peano

val tailAdd : Peano -> Peano -> Peano
val tailMult : Peano -> Peano -> Peano
val tailPow : Peano -> Peano -> Peano

val loop : ('a -> 'a) -> 'a -> Peano -> 'a

val loopAdd : Peano -> Peano -> Peano
val loopMult : Peano -> Peano -> Peano
val loopPow : Peano -> Peano -> Peano
