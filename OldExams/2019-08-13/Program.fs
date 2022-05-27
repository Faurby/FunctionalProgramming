open Question4

printfn "%A" <| mkTile "blue" "diamond"
printfn "%A" <| tileToString (Diamond, Blue)

let blueDiamond = mkTile "blue" "diamond" 
let blueStar = mkTile "blue" "star" 
let blueCircle = mkTile "blue" "circle" 
let redDiamond = mkTile "red" "diamond" 
let purpleDiamond = mkTile "purple" "diamond" 
let purpleStar = mkTile "purple" "star"

printfn "true = %A" <| validTiles [blueDiamond; redDiamond] purpleDiamond 
printfn "true = %A" <| validTiles [blueDiamond; blueCircle] blueStar 
printfn "false = %A" <| validTiles [blueDiamond; redDiamond] purpleStar
printfn "true = %A" <| validTiles2 [blueDiamond; redDiamond] purpleDiamond 
printfn "true = %A" <| validTiles2 [blueDiamond; blueCircle] blueStar 
printfn "false = %A" <| validTiles2 [blueDiamond; redDiamond] purpleStar
printfn "false = %A" <| validTiles2 [blueDiamond; blueCircle] blueDiamond