    let insert (s: string) di =
        let rec aux (s: string) (DI(booli, map2)) index =
            let c = s.Chars(index)
            if ((s.Length - 1) = index) then
                //Insert true
                let newMap = Map.empty<char, Dictionary2>
                newMap.Add(c, empty())
                
                DI(true, newMap)
                
            else
                //Create new DI
                //Insert (false, Map[A, ...)])
                //Insert (false, Map[A, (false, Map[P, ...])])
                
                if (index > 0) then
                    let lastChar = s.Chars(index - 1)
                    let DI(booli3, map3: Map<char, Dictionary2>) = Map.find lastChar map2
                    
                    
                    
                    
                    let newMap = Map.empty.Add(c, empty())
                    
                    
                    innerDI = DI(false, newMap)
                    map2.Add(lastChar, innerDI)
                    
                    aux s innerDI (index + 1)
                    
                else
                    
                    
                
                //Insert false and continue
                
                
                let newMap = map2.Add(c, empty())
                let newDI = DI(false, newMap)
                aux s newDI (index + 1)
                
        aux s di 0
