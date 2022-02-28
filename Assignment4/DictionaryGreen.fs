module DictionaryGreen

    type Dict = DI of Set<string>

    let empty (_ : unit) = DI(Set.empty<string>)

    let insert (s: string) (DI(set)) = DI(Set.add(s) set)

    let lookup (s: string) (DI(set)) = Set.contains(s) set
