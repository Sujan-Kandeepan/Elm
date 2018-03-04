import Html exposing (..)

main = text (toString "Cheat sheet")

-- Type Aliases
{-} type alias Location = { line : Int, column : Int} -}

-- Anonymous functions
{-} square = \n -> n^2 | squares = List.map (\n -> n^2) (List.rang 1 100) -}

-- Infix Operators
{-} viewNames1 names = String.join ", " (List.sort names) | viewNames2 names = names |> List.sort |> String.join ", " -}

-- Let Expressions
{-} let | ( three, four ) = ( 3, 4 ) | hypotenuse a b = sqrt (a^2 + b^2)
in | hypotenuse three four -}

-- List.Map
{-} map : (a -> b) -> List a -> List b | map sqrt [1,4,9] == [1,2,3] | map not [True,False,True] == [False,True,False] -}

-- Folds
{-} foldr f v list = case list of | x::xs -> f x (foldr f v xs) | [] -> v
foldl f v list = case list of | x::xs -> f (foldl f v xs) xs | [] -> v | -}

-- Other list functions
{-} length [1,2,3] == 3 | reverse [1..4] == [4,3,2,1] | take 2 [1,2,3,4] == [1,2]
drop 2 [1,2,3,4] == [3,4] | append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
append ['a','b'] ['c'] == ['a','b','c'] | concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
partition (\x -> x < 3) [0..5] == ([0,1,2], [3,4,5])
partition isEven        [0..5] == ([0,2,4], [1,3,5]) -}

-- String functions
{-} length "innumerable" == 11 | reverse "stressed" == "desserts" | repeat 3 "ha" == "hahaha"
append "butter" "fly" == "butterfly" | concat ["never","the","less"] == "nevertheless"
split "," "cat,dog,cow"  == ["cat","dog","cow"] | join "a" ["H","w","ii","n"]  == "Hawaiian"
slice  7  9 "snakes on a plane!" == "on" | contains "the" "theory" == True | String.toInt "123" == Ok 123 | String.toFloat "123" == Ok 123.0 | toList "abc" == ['a','b','c'] -}

-- Array functions
{-} length (fromList [1,2,3]) == 3
append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
get  0 (fromList [0,5,3]) == Just 0 | set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2] | toList (fromList [3,5,8]) == [3,5,8] -}

-- Shape and text
{-} rect 100 100 |> outlined (solid 5) red |> move (x,y) |> fadeIn t 0,
text "(Image)" |> size 18 |> centered |> filled black |> move (x,y-5) |> fadeIn t 0
curve (0,0) [Pull (0,10) (0,20)]  | type Pull = Pull (Float, Float) (Float, Float) -}

-- State diagrams
{-} states = [(SayA, (-300, 300)) ,(SayB, (-200,300)) ,(SayC, (-100,300))]
transitions = [(reciteAll, "Pause", [((SayA), (-250,250)) ,((SayB), (-150,250)) ,((SayC), (-50,250)) -}
