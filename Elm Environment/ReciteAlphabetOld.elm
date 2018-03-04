import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)

type Msg = Tick Float GetKeyState

type State = SayA | SayB | SayC | SayD | SayE | SayF | SayG | SayH | SayI | SayJ | SayK | SayL | SayM
           | SayN | SayO | SayP | SayQ | SayR | SayS | SayT | SayU | SayV | SayW | SayX | SayY | SayZ

model = {
          tick = 0
        , state =  SayA
        , transition = (SayZ, "change")
        }

states =
    [    (SayA, (-300, 300)) 
    ,    (SayB, (-200,300))
    ,    (SayC, (-100,300))
    ,    (SayD, (0,300))
    ,    (SayE, (100,300))
    ,    (SayF, (200,300))
    ,    (SayG, (300,300))
    ,    (SayH, (-300,100))
    ,    (SayI, (-200,100))
    ,    (SayJ, (-100,100))
    ,    (SayK, (0,100))
    ,    (SayL, (100,100))
    ,    (SayM, (200,100))
    ,    (SayN, (300,100))
    ,    (SayO, (-250,-100))
    ,    (SayP, (-150,-100))
    ,    (SayQ, (-50,-100))
    ,    (SayR, (50,-100))
    ,    (SayS, (150,-100))
    ,    (SayT, (250,-100))
    ,    (SayU, (-250,-300))
    ,    (SayV, (-150,-300))
    ,    (SayW, (-50,-300))
    ,    (SayX, (50,-300))
    ,    (SayY, (150,-300))
    ,    (SayZ, (250,-300))
    ]

transitions =
    [
        (updateState, "pause",
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
        ,((SayZ), (-320,-100))])
    ]

updateState : State -> State
updateState t = case t of
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

update msg model = case msg of
                  Tick t getKeyState -> if model.tick >= 16 then
                  { model | tick = 0, state = (updateState model.state), transition = (model.state, "pause") }
                  else
                  { model | tick = model.tick + 1, state = model.state, transition = model.transition }



view model = collage 768 760 [viewStateDiagram states transitions (Just model.state) (Just model.transition)]

main = gameApp Tick {
                          model = model,
                          view = view,
                          update = update
                        }
