import Html
import GraphicSVG exposing (..)
import Anonymous exposing(..)
import Set exposing (..)
import Array exposing (..)

type Message = GameTick Float GetKeyState

main = gameApp GameTick
  {
    model = model,
    view = view,
    update = update
  }

model =
  {
    t = 0
  }

view model = collage 2500 1000
  [
    (text <| toString <| Array.slice 0 50 userList)
    |> GraphicSVG.size 36
    |> centered
    |> filled black
    |> move (0,400),

    (text <| toString <| Array.slice 50 85 userList)
    |> GraphicSVG.size 36
    |> centered
    |> filled black
    |> move (0,200),

    (text <| toString <| Array.slice 85 117 userList)
    |> GraphicSVG.size 36
    |> centered
    |> filled black,

    (text <| toString <| userList)
    |> GraphicSVG.size 12
    |> centered
    |> filled black
    |> move (0,-100),

    (text <| toString <| scoreList)
    |> GraphicSVG.size 24
    |> centered
    |> filled black
    |> move (0,-200)
  ]

update message model = case message of
  GameTick tick (getKeyState,changeP1,changeP2) -> {t = tick}

unique list = Set.toList <| Set.fromList list

generateUserList list = case list of
  (x1,x2,x3,x4)::xs -> [x1] ++ generateUserList2 list
  [] -> []

generateUserList2 list = case list of
  (x1, x2, x3, x4)::(y1, y2, y3, y4)::xs ->
    if y1 /= x1
      then [y1] ++ generateUserList2 ((y1, y2, y3, y4)::xs)
    else generateUserList2 ((y1, y2, y3, y4)::xs)
  otherwise -> []

userList = Array.fromList <| generateUserList <| Anonymous.userQuestionTimePass

calculateScore list = case list of
  (x1,x2,x3,x4)::xs ->
    if Just x1 == Array.get 0 userList
      then [(x3,x4)] ++ calculateScore xs
    else calculateScore xs
  otherwise -> []

scoreList = Array.fromList <| calculateScore <| Anonymous.userQuestionTimePass
