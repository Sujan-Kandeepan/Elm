import Html exposing (..)

main = text (toString (toDigitsReverse 123) )

toDigitsReverse : Int -> List Int
toDigitsReverse n =
    if n <= 0 then
        []
    else
        List.append [n%10] (toDigitsReverse (Basics.floor (n/10)))
