import GraphicSVG exposing (..)
import String

view model = collage 500 500
  [ rect 45 20
      |> outlined (solid 3) pink
      |> move (0, -75)
  , text (toString <| answer) |> size 12
      |> filled purple
  ]

answer = read Ready <| String.toList "xxxxxMISSISSIPPIxxxxx"

type Recognizer
  = Ready
  | OneM
  | OneI
  | OneS
  | TwoS
  | TwoI
  | ThreeS
  | FourS
  | ThreeI | OneA
  | OneP   | OneU
  | TwoP   | OneG
  | FourI  | TwoA

read state letters
  = case (letters, state) of
      ('M'::ls,Ready)  -> read OneM ls
      ('I'::ls,OneM)   -> read OneI ls
      ('S'::ls,OneI)   -> read OneS ls
      ('S'::ls,OneS)   -> read TwoS ls
      ('I'::ls,TwoS)   -> read TwoI ls
      ('S'::ls,TwoI)   -> read ThreeS ls
      ('S'::ls,ThreeS) -> read FourS ls
      ('I'::ls,FourS)  -> read ThreeS ls
      ('P'::ls,ThreeS) -> read OneP ls
      ('P'::ls,OneP)   -> read TwoP ls
      ('I'::ls,TwoP)   -> "I read Mississippi"
      ('A'::ls,FourS)  -> read OneA ls
      ('U'::ls,OneA)   -> read OneU ls
      ('G'::ls,OneU)   -> read OneG ls
      ('A'::ls,OneG)   -> "I read Mississauga"
      (c::ls,any)      -> read Ready ls
      ([]::ls,Ready)   -> "I didn't read any of the words I know"

main = gameApp Tick { model = init,
                      view = view,
                      update = update
                    }
