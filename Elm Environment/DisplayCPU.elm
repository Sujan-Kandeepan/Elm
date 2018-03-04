-- http://www.cas.mcmaster.ca/~anand/1JC3Pics/CPU.pdf
-- mm's this week go to chinnh@mcmaster.ca (Natalie)
module TryCPU exposing (..)

import RunCPU exposing (..)
import CPU exposing (..)

import Array exposing (Array)
import GraphicSVG exposing (..)
import List exposing(concat,map,map2,foldr,indexedMap,filter,concatMap)
import Time
import Set

--main = show <| (initialState, initialTinyData)

view model = collage 500 500 [
                               rect 500 500
                               |> filled darkCharcoal,

                               model.instrOutput |> move (0,-22),

                               rect 100 20
                               |> filled (if isHalted model.cpu then red else green)
                               |> move (35,4),

                               text "Next Instruction"
                               |> size 12
                               |> centered
                               |> customFont ("Helvetica")
                               |> filled (if isHalted model.cpu then white else black)
                               |> move (35,0)
                               |> notifyTap NextInstr,

                               text "Current Instruction"
                               |> size 12
                               |> centered
                               |> customFont ("Helvetica")
                               |> filled yellow
                               |> move (35,-180)
                               |> notifyTap NextInstr,

                               triangle 5
                               |> filled red
                               |> move (90,-177.5),

                               displayCPU model.cpu
                             ]

displayRegister registerNumber registerValue x y = group [
                                                           square 80
                                                           |> filled blue
                                                           |> move (x,y),

                                                           text ("Register " ++ toString(registerNumber))
                                                           |> size 12
                                                           |> centered
                                                           |> customFont ("Helvetica")
                                                           |> filled lightGreen
                                                           |> move (x-8,y+25),

                                                           text (toString(registerValue))
                                                           |> size 48
                                                           |> centered
                                                           |> customFont ("Helvetica")
                                                           |> filled white
                                                           |> move (x,y-22)
                                                         ]

displayCPU cpu = case cpu of
                   CPUState (r1,r2,r3,r4,r5,r6,r7,r8) instruction compare halt -> group [
                                                                                          displayRegister 1 r1 -175 180,
                                                                                          displayRegister 2 r2 -75 180,
                                                                                          displayRegister 3 r3 -175 60,
                                                                                          displayRegister 4 r4 -75 60,
                                                                                          displayRegister 5 r5 -175 -60,
                                                                                          displayRegister 6 r6 -75 -60,
                                                                                          displayRegister 7 r7 -175 -180,
                                                                                          displayRegister 8 r8 -75 -180
                                                                                        ]

type Msg = NextInstr

update NextInstr model =
  case model.cpu of

     CPUState regs curr cmp Nothing ->
       let ((newCpu,newDat),thisInstr) =
             case model.program curr of
               Just i  -> (executeOne i (model.cpu, model.dat),toString i)
               Nothing -> ((CPUState regs curr cmp (Just IllegalInstrAddress),model.dat),"illegal")
       in { model | cpu = newCpu, dat = newDat,
                    instrOutput = group [
                                          thisInstr
                                          |> text
                                          |> size 15
                                          |> customFont ("Helvetica")
                                          |> filled white
                                          |> move (100,-160),

                                          model.instrOutput
                                          |> move (0,20)
                                        ]
          }

     CPUState regs curr cmp (Just halt) -> model -- CPU has stopped

init = { cpu = initialState
       , dat = initialData
       , instrOutput = group []
       , program = mkProgram [ LoadImmediate 1 7
                             , LoadImmediate 2 3
                             , LoadImmediate 4 20
                             , LoadImmediate 5 4
                             , Add 1 2 1
                             , Compare 1 4
                             , Branch [LT] 5
                             , Halt
                             ]
       }

main = notificationsApp { model = init , view = view , update = update }

--main = show <| runProgram (mkProgram [LoadImmediate 1 7
--                                     ,Halt]
--                          )
--                          (initialState, initialTinyData)

--view = collage 600 600 <|
--  [text <| toString initialState]
