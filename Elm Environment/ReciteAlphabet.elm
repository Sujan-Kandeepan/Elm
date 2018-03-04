import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)
import String exposing (..)

type State = SayA | SayB | SayC | SayD | SayE | SayF | SayG
           | SayH | SayI | SayJ | SayK | SayL | SayM | SayN
           | SayO | SayP | SayQ | SayR | SayS | SayT
           | SayU | SayV | SayW | SayX | SayY | SayZ

type Sequence = All | Vowels | Consonants

states =
    [    (SayA, (-300, 300))
        ,(SayB, (-200,300))
        ,(SayC, (-100,300))
        ,(SayD, (0,300))
        ,(SayE, (100,300))
        ,(SayF, (200,300))
        ,(SayG, (300,300))
        ,(SayH, (-300,100))
        ,(SayI, (-200,100))
        ,(SayJ, (-100,100))
        ,(SayK, (0,100))
        ,(SayL, (100,100))
        ,(SayM, (200,100))
        ,(SayN, (300,100))
        ,(SayO, (-250,-100))
        ,(SayP, (-150,-100))
        ,(SayQ, (-50,-100))
        ,(SayR, (50,-100))
        ,(SayS, (150,-100))
        ,(SayT, (250,-100))
        ,(SayU, (-250,-300))
        ,(SayV, (-150,-300))
        ,(SayW, (-50,-300))
        ,(SayX, (50,-300))
        ,(SayY, (150,-300))
        ,(SayZ, (250,-300))
    ]

transitions =
    [
        (reciteAll, "Pause",
        [((SayA), (-250,250))
        ,((SayB), (-150,250))
        ,((SayC), (-50,250))
        ,((SayD), (50,250))
        ,((SayE), (150,250))
        ,((SayF), (250,250))
        ,((SayG), (0,175))
        ,((SayH), (-250,50))
        ,((SayI), (-150,50))
        ,((SayJ), (-50,50))
        ,((SayK), (50,50))
        ,((SayL), (150,50))
        ,((SayM), (250,50))
        ,((SayN), (0,-25))
        ,((SayO), (-200,-150))
        ,((SayP), (-100,-150))
        ,((SayQ), (0,-150))
        ,((SayR), (100,-150))
        ,((SayS), (200,-150))
        ,((SayT), (0,-225))
        ,((SayU), (-200,-350))
        ,((SayV), (-100,-350))
        ,((SayW), (0,-350))
        ,((SayX), (100,-350))
        ,((SayY), (200,-350))
        ,((SayZ), (-325,-300))]),

        (reciteVowels, "Skip to Vowel",
        [((SayA), (0,340))
        ,((SayB), (0,340))
        ,((SayC), (0,340))
        ,((SayE), (175,175))
        ,((SayF), (175,175))
        ,((SayG), (175,175))
        ,((SayI), (80,-25))
        ,((SayJ), (80,-25))
        ,((SayK), (80,-25))
        ,((SayL), (80,-25))
        ,((SayM), (80,-25))
        ,((SayO), (-125,-225))
        ,((SayP), (-125,-225))
        ,((SayQ), (-125,-225))
        ,((SayR), (-125,-225))
        ,((SayS), (-125,-225))
        ,((SayU), (50,-260))
        ,((SayV), (50,-260))
        ,((SayW), (50,-260))
        ,((SayY), (-345,-345))]),

        (reciteConsonants, "Skip to Consonant",
        [((SayD), (150,340))
        ,((SayH), (-200,175))
        ,((SayN), (200,-25))
        ,((SayT), (225,-225))
        ,((SayZ), (350,230))])
    ]

reciteAll : State -> State
reciteAll t =
    case t of
       SayA -> SayB
       SayB -> SayC
       SayC -> SayD
       SayD -> SayE
       SayE -> SayF
       SayF -> SayG
       SayG -> SayH
       SayH -> SayI
       SayI -> SayJ
       SayJ -> SayK
       SayK -> SayL
       SayL -> SayM
       SayM -> SayN
       SayN -> SayO
       SayO -> SayP
       SayP -> SayQ
       SayQ -> SayR
       SayR -> SayS
       SayS -> SayT
       SayT -> SayU
       SayU -> SayV
       SayV -> SayW
       SayW -> SayX
       SayX -> SayY
       SayY -> SayZ
       SayZ -> SayA


reciteVowels : State -> State
reciteVowels t =
    case t of
       SayA -> SayE
       SayB -> SayE
       SayC -> SayE
       SayD -> SayE
       SayE -> SayI
       SayF -> SayI
       SayG -> SayI
       SayH -> SayI
       SayI -> SayO
       SayJ -> SayO
       SayK -> SayO
       SayL -> SayO
       SayM -> SayO
       SayN -> SayO
       SayO -> SayU
       SayP -> SayU
       SayQ -> SayU
       SayR -> SayU
       SayS -> SayU
       SayT -> SayU
       SayU -> SayY
       SayV -> SayY
       SayW -> SayY
       SayX -> SayY
       SayY -> SayA
       SayZ -> SayA

reciteConsonants : State -> State
reciteConsonants t =
   case t of
       SayA -> SayB
       SayB -> SayC
       SayC -> SayD
       SayD -> SayF
       SayE -> SayF
       SayF -> SayG
       SayG -> SayH
       SayH -> SayJ
       SayI -> SayJ
       SayJ -> SayK
       SayK -> SayL
       SayL -> SayM
       SayM -> SayN
       SayN -> SayP
       SayO -> SayP
       SayP -> SayQ
       SayQ -> SayR
       SayR -> SayS
       SayS -> SayT
       SayT -> SayV
       SayU -> SayV
       SayV -> SayW
       SayW -> SayX
       SayX -> SayY
       SayY -> SayZ
       SayZ -> SayB

type Msg = Tick Float GetKeyState
         | AllButton
         | VowelsButton
         | ConsonantsButton

model = {
          tick = 0
        , state =  SayA
        , transition = (SayZ, "Pause")
        , sequence = All
        }

update msg model = case msg of
                  Tick t getKeyState -> if model.tick >= 25 then
                  { model | tick = 0,
                    state =
                      if model.sequence == All then (reciteAll model.state)
                      else if model.sequence == Vowels then (reciteVowels model.state)
                      else (reciteConsonants model.state),
                    transition =
                      if model.sequence == All then (model.state, "Pause")
                      else if model.sequence == Vowels then (model.state, "Skip to Vowel")
                      else case model.state of
                        SayD -> (model.state, "Skip to Consonant")
                        SayH -> (model.state, "Skip to Consonant")
                        SayN -> (model.state, "Skip to Consonant")
                        SayT -> (model.state, "Skip to Consonant")
                        SayZ -> (model.state, "Skip to Consonant")
                        otherwise ->(model.state, "Pause")}
                  else
                  { model | tick = model.tick + 1, state = model.state,
                  transition = model.transition }
                  AllButton -> {model | sequence = All}
                  VowelsButton -> {model | sequence = Vowels}
                  ConsonantsButton -> {model | sequence = Consonants}

view model = collage 825 825 [viewStateDiagram states transitions
                               (Just model.state) (Just model.transition),

                               text "RECITING THE ALPHABET"
                               |> bold |> size 24 |> filled black |> move (-190,385),

                               text "Press a button to change between reciting
                               all letters, just vowels or just consonants:"
                               |> filled black |> move (-340,360),

                               text "All Letters" |> filled black |> move (90,360),

                               rect 75 20 |> outlined (solid 3)
                               (if model.sequence == All then green else black)
                               |> move (115,365)
                               |> notifyTap AllButton,

                               text "Vowels" |> filled black |> move (188,360),

                               rect 75 20 |> outlined (solid 3)
                               (if model.sequence == Vowels then green else black)
                               |> move (205,365)
                               |> notifyTap VowelsButton,

                               text "Consonants" |> filled black |> move (268,360),

                               rect 75 20 |> outlined (solid 3)
                               (if model.sequence == Consonants then green else black)
                               |> move (295,365)
                               |> notifyTap ConsonantsButton
                             ]

main = gameApp Tick {
                          model = model,
                          view = view,
                          update = update
                        }
