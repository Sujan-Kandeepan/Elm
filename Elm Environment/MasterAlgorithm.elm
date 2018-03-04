-- Group members: Maryam Taofeek, Noor-ul-ain Alamgir, Ryan Nourbaran, Sujan Kandeepan, Tom Xu

import GraphicSVG exposing (..)
import Array

type Message = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
              | NextSlide
              | LastSlide

-- this is the main function, and for simple animations, you would only replace the view function, or edit it below

main = gameApp GameTick {
                            model = init
                        ,   view = view
                        ,   update = update
                        }

-- MODEL

init = {
              t = 0 ,
            idx = 0 ,
              p = False, -- Pause
              r = 1 , -- Rewind
              a = 1  -- Acceleration
        }

-- VIEW

view model = let t = model.t
                 slide = Maybe.withDefault default (Array.get model.idx slides)

             in collage 1000 500 (slide t ++ borders ++ navigators)

-- UPDATE

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) ->
                              if (getKeyState LeftArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = max (model.idx - 1) 0
                              }
                              else if (getKeyState RightArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = min (model.idx + 1) (Array.length slides - 1)
                              }
                              else if (getKeyState Space) == JustDown then
                              { model |
                                  p = not model.p
                              }
                              else if (getKeyState UpArrow) == JustDown then
                              { model |
                                  a = min (model.a * 2) 4
                              }
                              else if (getKeyState DownArrow) == JustDown then
                              { model |
                                  a = max (model.a / 2) 0.5
                              }
                              else if (getKeyState (Key "R")) == JustDown then
                              { model |
                                  r = -model.r
                              }
                              else if (getKeyState Backspace) == JustDown then
                              { model |
                                  t = 0
                              }
                              else if model.p then
                              model
                              else
                              { model |
                                       t = max (model.t + 2.5 * model.a * model.r) 0
                              }
    NextSlide -> { model |
    t   = 0 ,
    idx = min (model.idx + 1) (Array.length slides - 1)
  }
    LastSlide -> { model |
    t   = 0 ,
    idx = max (model.idx - 1) 0
  }

--- MISCELLANEOUS

default t = []

borders = [rect 5000 5000
              |> filled white
              |> move (3000,0),
           rect 5000 5000
              |> filled white
              |> move (-3000,0),
           rect 5000 5000
              |> filled white
              |> move (0,2750),
           rect 5000 5000
              |> filled white
              |> move (0,-2750)]

navigators = [ group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                      ] |> move (450,-200)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                    ] |> rotate (degrees 180)
                      |> move (-450,-200)
                      |> makeTransparent 0.5
                |> notifyTap LastSlide
            ]


-- FUNCTIONS

--<< So why do I see (t - 100) or whatever value so often? >>

--   Whenever I do that, I'm basically delaying what I want to happen
--   by that value. Is it measure in seconds, frames or what? What's the unit here?
--   To be honest, I don't know. It has a lot to do with the UPDATE function, and
--   what value for 'x' you are using for " t = model.t + x ".

disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 -- Makes things vanish off the screen!

loop t n = let y = toFloat (floor (t / n)) -- This function is how I make things loop!
           in t - y * n

appear x n =    if x > n then makeTransparent 1 else makeTransparent 0 -- Makes things suddenly appear on the screen!

fadeIn t n = makeTransparent (tranSin (t-n) 1)

fadeOut t n = makeTransparent (1 - (tranSin (t-n) 1))

trans t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
               then 0
            else Basics.min t y

tranSin t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
               then 0
            else if t/100 > pi/2 then y
            else sin(t/100) * y

drawLine t (x1,y1) (x2,y2) = line (x1,y1) (x1 + tranSin (t) (x2 - x1), y1 + tranSin (t) (y2 - y1))

-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.

slides = Array.fromList [title, algorithmsAre, algorithmUses, symbolists,
                         connectionists1, connectionists2, evolutionaries,
                         bayesians, analogizers, combine, maUses, milestone1,
                         milestone2, thankYou]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

-- ///////////////////////// TITLE SLIDE ///////////////////////// Prepared by Sujan Kandeepan
title t = [
             --Background
             rect 1000 500
             |> filled black,

             --Bulb effects
             bulbTitle t,

             circle (7.5*t-1000)
             |> filled yellow
             |> move (0,10),

             rect 1000 500
             |> filled darkCharcoal
             |> fadeIn t 200,

             --Main title
             text "The Master Algorithm"
             |> size 72
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled black
             |> move (-5,95)
             |> fadeIn t 400,

             text "The Master Algorithm"
             |> size 72
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled blue
             |> move (0,100)
             |> fadeIn t 400,

             --Subtitle
             text "How the Quest for the Ultimate Learning"
             |> size 30
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled darkGray
             |> move (0,50)
             |> fadeIn t 525,

             text "Machine Will Remake Our World"
             |> size 30
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled darkGray
             |> move (0,15)
             |> fadeIn t 525,

             -- Author name
             text "By Pedro Domingos"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled lightCharcoal
             |> move (0,-20)
             |> fadeIn t 650,

             -- Group member names flash
             text "Sujan"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (-300,-100)
             |> appear t 800
             |> disappear t 825,

             text "Ryan"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (-165,-100)
             |> appear t 825
             |> disappear t 850,

             text "Noor-ul-ain"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (0,-100)
             |> appear t 850
             |> disappear t 875,

             text "Tom"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (165,-100)
             |> appear t 875
             |> disappear t 900,

             text "Maryam"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (300,-100)
             |> appear t 900
             |> disappear t 925,

             -- Group member names appear
             text "Sujan"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (-300,-100)
             |> appear t 1000,

             text "Ryan"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (-165,-100)
             |> appear t 1000,

             text "Noor-ul-ain"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (0,-100)
             |> appear t 1000,

             text "Tom"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (165,-100)
             |> appear t 1000,

             text "Maryam"
             |> size 24
             |> customFont "Helvetica"
             |> bold
             |> centered
             |> filled yellow
             |> move (300,-100)
             |> appear t 1000
           ]

bulbTitle t = group [
                 -- Glass portion
                 circle 40
                 |> filled yellow
                 |> move (0,20),

                 rect 45 40
                 |> filled yellow
                 |> move (0,-15),

                 -- Curve down
                 circle 10
                 |> filled black
                 |> move (-25,-22.5),

                 circle 10
                 |> filled black
                 |> move (25,-22.5),

                 rect 10 10
                 |> filled black
                 |> move (-20,-30),

                 rect 10 10
                 |> filled black
                 |> move (20,-30),

                 -- Base and bottom coil
                 circle 12.5
                 |> filled gray
                 |> move (0,-45),

                 rect 25 10
                 |> filled lightGray
                 |> move (0,-30),

                 circle 5
                 |> filled lightGray
                 |> move (-12.5,-30),

                 circle 5
                 |> filled lightGray
                 |> move (12.5,-30),

                 rect 25 10
                 |> filled lightGray
                 |> move (0,-37.5),

                 circle 5
                 |> filled lightGray
                 |> move (-12.5,-37.5),

                 circle 5
                 |> filled lightGray
                 |> move (12.5,-37.5),

                 rect 25 10
                 |> filled lightGray
                 |> move (0,-45),

                 circle 5
                 |> filled lightGray
                 |> move (-12.5,-45),

                 circle 5
                 |> filled lightGray
                 |> move (12.5,-45),

                 -- Fade in, light ray animations
                 rect 100 120
                 |> filled black
                 |> fadeOut t 0
               ]

-- ///////////////////////// WHAT ALGORITHMS ARE (SLIDE 1) ///////////////////////// Prepared by Maryam Taofeek
algorithmsAre t = [ circle 3000
                |> filled green
                |> move (300 * cos(t/100), 300 * sin(t/100)) ,
                 circle 40
                |> filled gray
                |> move (200 * cos(t/80), 150 * sin(t/80)),
                triangle 40
                |> filled red
                |> move (100 * cos(t/80), 100 * sin(t/80)),
                rect 60 60
                |> filled orange
                |> move (100 * sin(t/80), 100 * cos(t/80)),
                text "WHAT"
                |> size 150
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled hotPink
                |> move (0, 125),
                text "ARE"
                |> size 150
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (0, -50),
                text "ALGORITHMS?"
                |> size 66
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled hotPink
                |> move (0, -200 ),
                rect 300 800
                |> filled blue
                |> move (-400, 100),
                 rect 300 800
                |> filled blue
                |> move ( 400, -100),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 225),
                text "101010110101010"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 200),
                 text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 175),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 150),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 125),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 100),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 75),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 50),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 25),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, 0),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -25),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -50),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -75),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -100),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -125),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -150),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -175),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -200),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -225),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-375, -250),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 225),
                text "101010110101010"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 200),
                 text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 175),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 150),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 125),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 100),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 75),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 50),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 25),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, 0),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -25),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -50),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -75),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -100),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -125),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -150),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -175),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -200),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -225),
                text "101010101101001"
                |> size 30
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (375, -250)
                ]

-- ///////////////////////// HOW ALGORITHMS ARE USED TODAY (SLIDE 2) ///////////////////////// Prepared by Maryam Taofeek
algorithmUses t = [ triangle 1200
                |> filled darkRed
                |> move (0,0) ,
                text "How Algorithms are used today?"
                |> size 25
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (0, 200),
                rect 250 200
                |> filled gray
                |> move (0,0),
                rect 225 175
                |> filled lightBlue
                |> move (0,0),
                text "Google"
                |> size 25
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled red
                |> move ( -10* cos(t/80), -50 * sin(t/80)),
                rect 30 40
                |> filled gray
                |> move (0,-115),
                rect 150 20
                |> filled gray
                |> move (0,-125),
                rect 175 250
                |> filled darkBlue
                |> move (-275,0),
                rect 160 230
                |> filled black
                |> move (-275,0),
                text "SAMSUNG"
                |> size 20
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-275, 70),
                rect 126 150
                |> filled blue
                |> move (-275, -20),
                text "Facebook"
                |> size 25
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled white
                |> move (-275, 10),
                 rect 175 250
                |> filled black
                |> move (275,0),
                rect 50 10
                |> filled darkBlue
                |> move (-275, -105),
                rect 100 50
                |> filled darkBlue
                |> move (275,70),
                 rect 150 150
                |> filled darkGray
                |> move (275,-40),
                 text "CASIO"
                |> size 20
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled darkGray
                |> move (275, 100),
                rect 20 10
                |> filled darkBlue
                |> move (300, 20),
                text "ON"
                |> size 16
                |> bold
                |> customFont "Helvetica"
                |> filled darkBlue
                |> move (310, 10),
                text "7 8 9 DEL AC"
                |> size 15
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled darkBlue
                |> move (275, -10),
                text "4   5   6  X  / SIN"
                |> size 15
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled darkBlue
                |> move (275, -40),
                 text "1 2 3 + - COS"
                |> size 15
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled darkBlue
                |> move (275, -70),
                 text "0 . EXP Ans ="
                |> size 15
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled darkBlue
                |> move (275, -100),
                text "4+5 = 9"
                |> size 15
                |> bold
                |> customFont "Helvetica"
                |> centered
                |> filled yellow
                |> move (275, 70)
                ]

-- ///////////////////////// SYMBOLISTS (SLIDE 3) ///////////////////////// Prepared by Sujan Kandeepan
symbolists t = [
              rect 1000 500
              |> filled darkCharcoal,

              banner t
              |> move (0,tranSin (t-800) 150),

              bullet t "Idea: all problems and intelligence can be reduced to symbol manipulation." 0 1000,

              bullet t "Symbolists use existing knowledge to learn from the available data." -75 1100,

              bullet t "Inverse deduction: find missing knowledge in order to make a deduction." -150 1200
           ]

banner t = group [
                    square t darkBrown -450,

                    symbol t "~" -450 250,

                    square t red -350,

                    symbol t "!" -350 275,

                    square t lightRed -250,

                    symbol t "@" -250 300,

                    square t orange -150,

                    symbol t "#" -150 325,

                    square t yellow -50,

                    symbol t "$" -50 350,

                    square t green 50,

                    symbol t "%" 50 375,

                    square t lightBlue 150,

                    symbol t "^" 150 400,

                    square t lightPurple 250,

                    symbol t "&" 250 425,

                    square t purple 350,

                    symbol t "*" 350 450,

                    square t charcoal 450,

                    symbol t "+" 450 475,

                    letter t "S" -450,

                    letter t "Y" -350,

                    letter t "M" -250,

                    letter t "B" -150,

                    letter t "O" -50,

                    letter t "L" 50,

                    letter t "I" 150,

                    letter t "S" 250,

                    letter t "T" 350,

                    letter t "S" 450
                 ]

square t colour x = group [
                            rect 100 100
                            |> filled colour
                            |> move (tranSin (t-100) x,0)
                            |> fadeIn t 0
                          ]

symbol t txt x enter = group [
                                text txt
                                |> size 48
                                |> customFont "Helvetica"
                                |> bold
                                |> centered
                                |> filled white
                                |> move (x,-15)
                                |> fadeIn t enter
                                |> fadeOut t 600
                              ]

letter t txt x = group [
                         text txt
                         |> size 48
                         |> customFont "Helvetica"
                         |> bold
                         |> centered
                         |> filled white
                         |> move (x,-15)
                         |> fadeIn t 600
                       ]

bullet t txt y enter = group [
                          text ("- " ++ txt)
                          |> size 22
                          |> customFont "Helvetica"
                          |> bold
                          |> filled white
                          |> move (-400,y)
                          |> fadeIn t enter
                       ]

-- ///////////////////////// CONNECTIONISTS (SLIDE 4) ///////////////////////// Prepared by Tom Xu
connectionists1 t = [ -- Connectionist
             rect 2000 2000
                |> filled hotPink,


             wedge 390 0.5
                |> filled pink
                |> move (0,-145)
                |> rotate (pi/2)
                |> fadeIn t 200 ,

             pinkBrain
                |> fadeOut t 200
                |> move (40,0),

             whiteBrain
                |> fadeIn t 200
                |> move (40,0),

             text "Connectionists"
                |> bold
                |> size 80
                |> filled pink
                |> move (-270, -230)
                |> fadeIn t 200 ]

connectionists2 t = [ roundedRect 50 50 5 --connectionists slide 2
                |> filled blue
                |> move (-200, 200),

             roundedRect 50 50 5
                |> filled blue
                |> move (-300, 50),

             roundedRect 50 50 5
                |> filled blue
                |> move (-100, 50),

             roundedRect 50 50 5
                |> filled blue
                |> move (-400, -100),

             roundedRect 50 50 5
                |> filled blue
                |> move (-200, -100),

             roundedRect 50 50 5
                |> filled blue
                |> move (0, -100),

             drawLine t (-200,200) (-300,50)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

             drawLine t (-200,200) (-100,50)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

             drawLine (t-100) (-300,50) (-400,-100)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

             drawLine (t-100) (-300,50) (-200,-100)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

             drawLine (t-100) (-100,50) (-200,-100)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

             drawLine (t-100) (-100,50) (0,-100)
               |> outlined (solid 5) blue
               |> fadeOut t 300,

              text "Computer Architecture"
              |> bold
              |> size 35
              |> filled blue
              |> move (70,100)
              |> fadeOut t 300,

              text "Neurons"
              |> bold
              |> size 35
              |> filled blue
              |> move (70,100)
              |> fadeIn t 400,

              drawLine (t-400) (-200,200) (-300,50)
               |> outlined (solid 5) blue,

             drawLine (t-400) (-200,200) (-100,50)
               |> outlined (solid 5) blue,

             drawLine (t-400) (-300,50) (-400,-100)
               |> outlined (solid 5) blue,

             drawLine (t-400) (-300,50) (-200,-100)
               |> outlined (solid 5) blue,

             drawLine (t-400) (-100,50) (-200,-100)
               |> outlined (solid 5) blue,

             drawLine (t-400) (-100,50) (0,-100)
               |> outlined (solid 5) blue,

               drawLine (t-400) (-200,200) (0,-100)
               |> outlined (solid 5) blue,

               drawLine (t-400) (-300,50) (-100,50)
               |> outlined (solid 5) blue,

               drawLine (t-400) (-400,-100) (-200,-100)
               |> outlined (solid 5) blue,

                  drawLine (t-400) (0,-100) (-200,-100)
               |> outlined (solid 5) blue

               ]

pinkBrain = group [roundedRect 70 20 5
                   |> filled pink
                   |> move (-330,-121),

                  circle 20
                  |> filled pink
                  |> move (-330,-80),

                  roundedRect 20 80 5
                  |> filled pink
                  |> move (-330,-13),

                  roundedRect 20 100 5
                  |> filled pink
                  |> move (-100,150),

                  roundedRect 150 20 5
                  |> filled pink
                  |> move (-36,200),

                  circle 20
                  |> filled pink
                  |> move (65,200),

                  pinkPart2
                  |> move (100, -230)
                  --|> mirrorX
                  |> rotate (3*pi/2),

                  pinkPart1,

                  pinkPart1
                  |> move (70,-120)
                  |> rotate (3*pi/2) ]

pinkPart1 = group [circle 20
                   |> filled pink
                   |> move (-200,150),

                   roundedRect 20 100 5
                   |> filled pink
                   |> move (-148,98)
                   |> rotate (pi/4),

                   roundedRect 20 150 5
                   |> filled pink
                   |> move (-118,0),

                   circle 20
                   |> filled pink
                   |> move (-180,70),

                   roundedRect 20 150 5
                   |> filled pink
                   |> move (-180,-28),

                   roundedRect 170 20 5
                   |> filled pink
                   |> move (-100,-121),

                   circle 20
                   |> filled pink
                   |> move (-212,-120),

                   circle 20
                   |> filled pink
                   |> move (-270,-120),

                   roundedRect 20 100 5
                   |> filled pink
                   |> move (-270,-45),

                   roundedRect 20 120 5
                   |> filled pink
                   |> move (-242,51)
                   |> rotate (5*pi/6)
              ]

pinkPart2 = group [circle 20
                   |> filled pink
                   |> move (-200,150),

                   roundedRect 20 100 5
                   |> filled pink
                   |> move (-148,98)
                   |> rotate (pi/4),

                   roundedRect 20 150 5
                   |> filled pink
                   |> move (-118,0),

                   circle 20
                   |> filled pink
                   |> move (-180,70),

                   roundedRect 20 150 5
                   |> filled pink
                   |> move (-180,-28)]

whiteBrain = group [roundedRect 70 20 5
                    |> filled hotPink
                    |> move (-330,-121),

                    circle 20
                    |> filled hotPink
                    |> move (-330,-80),

                    roundedRect 20 80 5
                    |> filled hotPink
                    |> move (-330,-13),

                    roundedRect 20 100 5
                    |> filled hotPink
                    |> move (-100,150),

                    roundedRect 150 20 5
                    |> filled hotPink
                    |> move (-36,200),

                    circle 20
                    |> filled hotPink
                    |> move (65,200),

                    whitePart2
                    |> move (100, -230)
                    --|> mirrorX
                    |> rotate (3*pi/2),

                    whitePart1,

                    whitePart1
                    |> move (70,-120)
                    |> rotate (3*pi/2) ]

whitePart1 = group [circle 20
                    |> filled hotPink
                    |> move (-200,150),

                    roundedRect 20 100 5
                    |> filled hotPink
                    |> move (-148,98)
                    |> rotate (pi/4),

                    roundedRect 20 150 5
                    |> filled hotPink
                    |> move (-118,0),

                    circle 20
                    |> filled hotPink
                    |> move (-180,70),

                    roundedRect 20 150 5
                    |> filled hotPink
                    |> move (-180,-28),

                    roundedRect 170 20 5
                    |> filled hotPink
                    |> move (-100,-121),

                    circle 20
                    |> filled hotPink
                    |> move (-212,-120),

                    circle 20
                    |> filled hotPink
                    |> move (-270,-120),

                    roundedRect 20 100 5
                    |> filled hotPink
                    |> move (-270,-45),

                    roundedRect 20 120 5
                    |> filled hotPink
                    |> move (-242,51)
                    |> rotate (5*pi/6)
                  ]

whitePart2 = group [circle 20
                    |> filled hotPink
                    |> move (-200,150),

                    roundedRect 20 100 5
                    |> filled hotPink
                    |> move (-148,98)
                    |> rotate (pi/4),

                    roundedRect 20 150 5
                    |> filled hotPink
                    |> move (-118,0),

                    circle 20
                    |> filled hotPink
                    |> move (-180,70),

                    roundedRect 20 150 5
                    |> filled hotPink
                    |> move (-180,-28)]

-- ///////////////////////// EVOLUTIONARIES (SLIDE 5) ///////////////////////// Prepared by Noor-ul-ain Alamgir
evolutionaries t = [ rect 1500 1500
                |> filled charcoal
                |> move (100 * cos(t/100), 100 * sin(t/100)) ,
                drawLine (t-100) (20,175) (20,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (40,175) (40,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (60,175) (60,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (80,175) (80,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (100,175) (100,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (120,175) (120,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (140,175) (140,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (160,175) (160,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (180,175) (180,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (200,175) (200,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (220,175) (220,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (240,175) (240,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (260,175) (260,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (280,175) (280,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (300,175) (300,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (320,175) (320,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (340,175) (340,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (360,175) (360,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (380,175) (380,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (0,175) (0,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-20,175) (-20,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-40,175) (-40,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-60,175) (-60,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-80,175) (-80,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-100,175) (-100,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-120,175) (-120,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-140,175) (-140,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-160,175) (-160,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-180,175) (-180,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-200,175) (-200,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-220,175) (-220,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-240,175) (-240,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-260,175) (-260,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-280,175) (-280,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-300,175) (-300,205)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-320,175) (-320,205)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-340,175) (-340,205)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-360,175) (-360,205)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-380,175) (-380,205)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                --
                drawLine (t-100) (300,205) (300,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (320,205) (320,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (340,205) (340,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (360,205) (360,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (380,205) (380,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (200,205) (200,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (220,205) (220,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (240,205) (240,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (260,205) (260,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (280,205) (280,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (100,205) (100,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (120,205) (120,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (140,205) (140,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (160,205) (160,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (180,205) (180,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (20,205) (20,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (40,205) (40,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (60,205) (60,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (80,205) (80,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (0,205) (0,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-20,205) (-20,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-40,205) (-40,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-60,205) (-60,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-80,205) (-80,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-100,205) (-100,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-120,205) (-120,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-140,205) (-140,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-160,205) (-160,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-180,205) (-180,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-200,205) (-200,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-220,205) (-220,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-240,205) (-240,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-260,205) (-260,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-280,205) (-280,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-300,205) (-300,235)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-320,205) (-320,235)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-340,205) (-340,235)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-360,205) (-360,235)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-380,205) (-380,235)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-50) (-400, 235) (400, 235)
                |> outlined (solid 4) white
                |> move (-1000 + (tranSin (t/5) 1000),0),
                 drawLine (t-50) (-400, 175) (400, 175)
                |> outlined (solid 4) white
                |> move (1000 + (tranSin (t/5) -1000),0),
                text "EVOLUTIONARIES"
                |> size 80
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled lightGreen
                |> addOutline (solid 2) black
                |> move (0,0)
                |> fadeIn t 10
                |> fadeOut t 600,
                text "EVOLUTIONARIES"
                |> size 80
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled lightGreen
                |> addOutline (solid 2) black
                |> move (0,100)
                |> fadeIn t 800,
                text "genetic programming"
                |> size 40
                |> bold
                |> customFont "Times New Roman"
                |> filled white
                |> move (-350,25)
                |> fadeIn t 850,
                text "simulate evolution on the computer and"
                |> size 40
                |> bold
                |> customFont "Times New Roman"
                |> filled white
                |> move (-350,-75)
                |> fadeIn t 900,
                text "draw on genetics and evolutionary biology"
                |> size 40
                |> bold
                |> customFont "Times New Roman"
                |> filled white
                |> move (-350,-125)
                |> fadeIn t 900,
                circle 10
                |> filled hotPink
                |> move (-400, -65)
                |> fadeIn t 900,
                circle 10
                |> filled hotPink
                |> move (-400, 35)
                |> fadeIn t 900,
                drawLine (t-100) (20,-175) (20,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (40,-175) (40,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (60,-175) (60,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (80,-175) (80,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (100,-175) (100,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (120,-175) (120,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (140,-175) (140,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (160,-175) (160,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (180,-175) (180,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (200,-175) (200,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (220,-175) (220,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (240,-175) (240,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (260,-175) (260,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (280,-175) (280,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (300,-175) (300,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (320,-175) (320,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (340,-175) (340,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (360,-175) (360,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (380,-175) (380,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (0,-175) (0,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-20,-175) (-20,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-40,-175) (-40,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-60,-175) (-60,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-80,-175) (-80,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-100,-175) (-100,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-120,-175) (-120,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-140,-175) (-140,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-160,-175) (-160,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-180,-175) (-180,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-200,-175) (-200,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-220,-175) (-220,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-240,-175) (-240,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-260,-175) (-260,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-280,-175) (-280,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-300,-175) (-300,-205)
                |> outlined (solid 4) lightBrown
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-320,-175) (-320,-205)
                |> outlined (solid 4) green
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-340,-175) (-340,-205)
                |> outlined (solid 4) hotPink
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-360,-175) (-360,-205)
                |> outlined (solid 4) orange
                |> move (1000 + (tranSin (t/5) -1000),0),
                drawLine (t-100) (-380,-175) (-380,-205)
                |> outlined (solid 4) lightBlue
                |> move (1000 + (tranSin (t/5) -1000),0),
                --
                drawLine (t-100) (300,-205) (300,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (320,-205) (320,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (340,-205) (340,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (360,-205) (360,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (380,-205) (380,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (200,-205) (200,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (220,-205) (220,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (240,-205) (240,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (260,-205) (260,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (280,-205) (280,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (100,-205) (100,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (120,-205) (120,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (140,-205) (140,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (160,-205) (160,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (180,-205) (180,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (20,-205) (20,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (40,-205) (40,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (60,-205) (60,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (80,-205) (80,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (0,-205) (0,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-20,-205) (-20,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-40,-205) (-40,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-60,-205) (-60,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-80,-205) (-80,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-100,-205) (-100,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-120,-205) (-120,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-140,-205) (-140,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-160,-205) (-160,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-180,-205) (-180,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-200,-205) (-200,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-220,-205) (-220,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-240,-205) (-240,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-260,-205) (-260,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-280,-205) (-280,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-300,-205) (-300,-235)
                |> outlined (solid 4) green
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-320,-205) (-320,-235)
                |> outlined (solid 4) orange
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-340,-205) (-340,-235)
                |> outlined (solid 4) lightBlue
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-360,-205) (-360,-235)
                |> outlined (solid 4) lightBrown
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-100) (-380,-205) (-380,-235)
                |> outlined (solid 4) hotPink
                |> move (-1000 + (tranSin (t/5) 1000),0),
                drawLine (t-50) (-400, -235) (400, -235)
                |> outlined (solid 4) white
                |> move (1000 + (tranSin (t/5) -1000),0),
                 drawLine (t-50) (-400, -175) (400, -175)
                |> outlined (solid 4) white
                |> move (-1000 + (tranSin (t/5) 1000),0)
  ]

-- ///////////////////////// BAYESIANS (SLIDE 6) ///////////////////////// Prepared by Ryan Nourbaran
bayesians t =  [rect 1000 1000
            |> filled optGold
           ,

-- Equation
           equation (loop (t) 1600)
             |> fadeIn t 0
          ,
--Chart
             group [group  [outlined (solid 3) black (openPolygon [(-200,150),(-200,-200)])
           ,  outlined (solid 3) black (openPolygon [(-200,-200),(160,-200)]
           )
           ,
             bars (t-600)  [
                        (3528,1),
                        (13874,5),
                        (10707,4),
                        (8295,3),
                        (6142, 2)]
                 |> move (-225,75)]
                 |> fadeIn t 600]
                 |> scale 1.5
                 |> scale (0.9-(tranSin (t-900) 0.28))
                 |> move (0-(tranSin (t-900) 280), (40+(tranSin (t-900) 40)))

              ,
--Arrow
              group [
              drawLine (t-700) (-270,-170) (170,180)
                  |> outlined (solid 20) red
                  |> appear t 700
                , group [
                line  (-270,-160) (-270,-220)
                  |> outlined (solid 20) red
                , line  (-270,-170) (-320,-170)
                  |> outlined (solid 20) red]
                  |> appear t 700
                  |> move (0 +(tranSin (t-700) 440), 0 + (tranSin (t-700) 350))]
                |> scale (0.9-(tranSin (t-900) 0.28))
                |> move (0-(tranSin (t-900) 280), (40+(tranSin (t-900) 40)))

              ,
              text "Bayesians"
                |> size 80
                |> bold
                |> centered
                |> filled black
                |> move (0,-180)
                |> fadeIn t 50

               ]
--equation helper function
equation t = group [
          rectangle 560 200
         |> filled optBlue
         |> addOutline (solid 2) black
         |> move (260,20)
         |> fadeIn t 100
         |> fadeOut t 1500
         ,
          text "P( H | E ) ="
         |> size 45
         |> customFont "David"
         |> filled black
         |> fadeIn t 100
         |> fadeOut t 1500
         ,
          text "P( E | H ) x P( H )"
         |> size 45
         |> customFont "David"
         |> filled black
         |> move (220,30)
         |> fadeIn t 200
         |> fadeOut t 1400
         ,
          drawLine (t-300)(210,20) (530,20)
         |> outlined (solid 4) black
         |> fadeOut t 1300
         ,
          text "P( E )"
         |> size 45
         |> customFont "David"
         |> filled black
         |> move (320,-30)
         |> fadeIn t 400
         |> fadeOut t 1200
         ]
         |> move (-60,0)

--bar graph helper function
bars t l =
                   group (List.map (makeLabels t) l)


makeLabels t (h,n) = group [rectangle 60 (h/40)
                                    |> filled blue
                                    |> move (0,-275+(h/80))
                                   ]
                                        |> move (70 * n,0)
                                        |> fadeIn t (n * 16)

--COLOURS
optRed = rgb 211 94 95
optBlue = rgb 40 110 250
optGreen = rgb 12 200 11
optOrange = rgb 245 111 66
optPurple = rgb 194 73 197
optGold = rgb 204 194 30
optGray = rgb 128 133 133

-- ///////////////////////// ANALOGIZERS (SLIDE 7) ///////////////////////// Prepared by Noor-ul-ain Alamgir
analogizers t = [  rect 1500 1500
                |> filled charcoal
                |> move (100 * cos(t/100), 100 * sin(t/100)),
                rect 600 1000
                |> filled charcoal
                |> move (300, 100),
               rect 400 500
                |> filled charcoal
                |> move (200, 100),
                rect 300 200
                |> filled charcoal
                |> move (-320, 120),
                circle 20
                |> filled yellow
                |> move (-320, 190)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, 185)
                |> fadeIn t 500,
                circle 20
                |> filled red
                |> move (-320, 120)
                |> fadeIn t 500,
                text "C"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, 115)
                |> fadeIn t 500,
                drawLine (t-700) (-320,170) (-320,140)
                |> outlined (solid 3) black,
                drawLine (t-700) (-320,100) (-320,60)
                |> outlined (solid 3) black,
                 drawLine (t-700) (-300,115) (-200,115)
                |> outlined (solid 3) black,
                drawLine (t-700) (-340,115) (-450,115)
                |> outlined (solid 3) black,
                circle 20
                |> filled yellow
                |> move (-320, 50)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, 45)
                |> fadeIn t 500,
                circle 20
                |> filled yellow
                |> move (-200, 120)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-200, 115)
                |> fadeIn t 500,
                circle 20
                |> filled yellow
                |> move (-440, 120)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-440, 115)
                |> fadeIn t 500,
                circle 20
                |> filled yellow
                |> move (-320, -190)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, -195)
                |> fadeIn t 500,
                circle 20
                |> filled red
                |> move (-320, -120)
                |> fadeIn t 500,
                text "C"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, -125)
                |> fadeIn t 500,
                drawLine (t-700) (-300,-125) (-200,-125)
                |> outlined (solid 3) black,
                drawLine (t-700) (-340,-125) (-450,-125)
                |> outlined (solid 3) black,
                drawLine (t-700) (-320,-100) (-320,-50)
                |> outlined (solid 3) black,
                drawLine (t-700) (-320,-140) (-320,-170)
                |> outlined (solid 3) black,
                circle 20
                |> filled yellow
                |> move (-320, -50)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-320, -55)
                |> fadeIn t 500,
                circle 20
                |> filled blue
                |> move (-200, -120)
                |> fadeIn t 500,
                text "O"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-200, -125)
                |> fadeIn t 500,
                circle 20
                |> filled yellow
                |> move (-200, -50)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-200, -55)
                |> fadeIn t 500,
                circle 20
                |> filled yellow
                |> move (-440, -120)
                |> fadeIn t 500,
                text "H"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled black
                |> move (-440, -125)
                |> fadeIn t 500,
                 drawLine (t-700) (-200,-70) (-200,-100)
                |> outlined (solid 3) black,
                 text "Methanol"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled orange
                |> move (-250, -200)
                |> fadeIn t 600,
                text "Methane"
                |> size 20
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled orange
                |> move (-400, 200)
                |> fadeIn t 600,
                rect 32 400
                |> filled lightBlue
                |> move (-100,0)
                |> fadeIn t 400,
                rect 32 400
                |> filled lightBlue
                |> move (100,0)
                |> fadeIn t 400,
                rect 32 250
                |> filled orange
                |> move (-50,0)
                |> fadeIn t 400,
                rect 32 250
                |> filled orange
                |> move (50,0)
                |> fadeIn t 400,
                 rect 20 80
                |> filled orange
                |> move (0,-165)
                |> move (-700 + (tranSin (t/2) 700),0)
                |> fadeOut t 500,
                rect 50 50
                |> filled blue
                |> move (0,-85)
                |> move (700 + (tranSin (t/2) -700),0)
                |> fadeOut t 500,
                rect 50 50
                |> filled lightBrown
                |> move (0,-25)
                |> move (-700 + (tranSin (t/2) 700),0)
                |> fadeOut t 500,
                rect 50 60
                |> filled red
                |> move (0,45)
                |> move (700 + (tranSin (t/2) -700),0)
                |> fadeOut t 500,
                rect 30 50
                |> filled pink
                |> move (0,118)
                |> move (-700 + (tranSin (t/2) 700),0)
                |> fadeOut t 500,
                rect 30 60
                |> filled lightYellow
                |> move (0, 200)
                |> move (700 + (tranSin (t/2) -700),0)
                |> fadeOut t 500,
                 text "A"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,200)
                |> fadeIn t 10,
                text "N"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0, 158)
                |> fadeIn t 20,
                text "A"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,118)
                |> fadeIn t 30,
                text "L"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,75)
                |> fadeIn t 40,
                text "O"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,32)
                |> fadeIn t 50,
                text "G"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,-15)
                |> fadeIn t 60,
                text "I"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> move (0,-57)
                |> addOutline (solid 1) darkBlue
                |> fadeIn t 70,
                text "Z"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,-100)
                |> fadeIn t 80,
                text "E"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> move (0,-142)
                |> addOutline (solid 1) darkBlue
                |> fadeIn t 90,
                text "R"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,-185)
                |> fadeIn t 100,
                text "S"
                |> size 55
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled green
                |> addOutline (solid 1) darkBlue
                |> move (0,-230)
                |> fadeIn t 110,
                 drawLine (t-800) (200,175) (200,-200)
                |> outlined (solid 3) white,
                drawLine (t-800) (150,-175) (475,-175)
                |> outlined (solid 3) white,
                drawLine (t-800) (200,-175) (250,0)
                |> outlined (solid 3) yellow,
                drawLine (t-800) (200,-175) (475, 50)
                |> outlined (solid 3) yellow,
                drawLine (t-800) (250, 0) (475, 50)
                |> outlined (solid 3) orange,
                circle 5
                |> filled green
                |> move (250, 0)
                |> fadeIn t 800,
                circle 5
                |> filled green
                |> move (475, 50)
                |> fadeIn t 800,
                text "^"
                |> size 30
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (200, 155)
                |> fadeIn t 800,
                text ">"
                |> size 25
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (473, -183)
                |> fadeIn t 800,
                text "θ"
                |> size 25
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (235, -130)
                |> fadeIn t 900,
                text "A"
                |> size 25
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (235, 10)
                |> fadeIn t 900,
                text "B"
                |> size 25
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled white
                |> move (465, 60)
                |> fadeIn t 900,
                text "d"
                |> size 25
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled pink
                |> move (350, 25)
                |> fadeIn t 900,
                text "Euclidean Distance"
                |> size 30
                |> bold
                |> customFont "Times New Roman"
                |> centered
                |> filled pink
                |> move (350, 175)
                |> fadeIn t 1000
                ]

-- ///////////////////////// COMBINING TO FORM MASTER ALGORITHM (SLIDE 8) ///////////////////////// Prepared by Ryan Nourbaran
combine t = [
--background
             rectangle 5000 5000
             |> filled optOrange
             ,
--piece topL (1) loop
              piece1 (loop t 1300)
                ,
--piece topmid (2) loop
              piece2 (loop t 1300)
                ,
--piece MidL (3) loop
              piece3 (loop t 1300)
               ,
--piece midR (4) loop

              piece4 (loop t 1300)
--piece botM (5) loop
              ,
              piece5 (loop t 1300)
--piece topR (6) loop
              ,
              piece6 (loop t 1300)
--piece botL (7) loop
              ,
              piece7 (loop t 1300)
--piece botR (8) loop
              ,
              piece8 (loop t 1300)
--master algorithm peice appears (MAKE THIS BETTER, MORE PUZZLE-LIKE)
              ,
              mA (loop t 1300)

                  ]
--topL piece helper
piece1 t = group [
                   rect 160 160
              |> filled red
              |> addOutline (solid 4) black
              ,
                   circle 28
              |> filled optOrange
              |> move (59,0)
              |> addOutline (solid 4) black
              ,
                  rectangle 27 60
              |> filled optOrange
              |> move (95,0)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,-59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, -95)
              ,
              text "Symbolists"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> move (-80,10)]
              |> move (-500,0)
              |> move (-160+(tranSin (t-100) 500),80)
              |> move (0+(tranSin (t-1000) -500),80)

--topM piece helper
piece2 t = group [
                   rect 160 160
              |> filled optBlue
              |> addOutline (solid 4) black
              ,
                    circle 28
              |> filled optBlue
              |> addOutline (solid 4) black
              |> move (0,100)
              ,
                    rectangle 60 25
              |> filled optBlue
              |> move (0, 65)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,-59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, -95)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (-59,0)
              ,
                    rectangle 25 60
              |> filled optOrange
              |> move (-95, 0)
              ,
              text "Bayesians"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> rotate (degrees -90)
                |> move (-10,78)]

              |> move (0,500)
              |> rotate (degrees 90)
              |> move (0,160-(tranSin (t-500) 500))
              |> move (0,0-(tranSin (t-1200) -500))

--midR piece helper
piece3 t = group [
                   rect 160 160
              |> filled optBlue
              |> addOutline (solid 4) black
              ,
                    circle 28
              |> filled optBlue
              |> addOutline (solid 4) black
              |> move (0,100)
              ,
                    rectangle 60 25
              |> filled optBlue
              |> move (0, 65)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,-59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, -95)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (-59,0)
              ,
                    rectangle 25 60
              |> filled optOrange
              |> move (-95, 0)
              ,
              text "Bayesians"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> mirrorX
                |> move (78,-10)]

              |> move (-500,0)
              |> rotate (degrees 180)
              |> mirrorY
              |> move (-160+(tranSin (t-500) 500),0)
              |> move (0+(tranSin (t-1200) -500),0)

--midR piece helper
piece4 t = group [
                   rect 160 160
              |> filled optBlue
              |> addOutline (solid 4) black
              ,
                   circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, 94)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,-59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, -95)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (-59,0)
              ,
                    rectangle 25 60
              |> filled optOrange
              |> move (-95,0)
              ,
              text "Bayesians"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> move (-30,-10)]

              |> move (500,0)
              |> move (160-(tranSin (t-500) 500),0)
              |> move (0-(tranSin (t-1200) -500),0)

--botM piece helper
piece5 t = group [
                   rect 160 160
              |> filled optBlue
              |> addOutline (solid 4) black
              ,
                   circle 28
              |> filled optBlue
              |> move (-100,0)
              |> addOutline (solid 4) black
              ,
                  rectangle 27 60
              |> filled optBlue
              |> move (-64,0)
              ,
                   circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, 94)
              ,
                    circle 28
              |> filled optOrange
              |> addOutline (solid 4) black
              |> move (0,-59)
              ,
                    rectangle 60 25
              |> filled optOrange
              |> move (0, -95)
              ,
              text "Bayesians"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> rotate (degrees 90)
                |> move (-50,-50)]

              |> rotate (degrees -90)
              |> move (0,-500)
              |> move (0,-160+(tranSin (t-500) 500))
              |> move (0,0+(tranSin (t-1200) -500))

--topR piece helper
piece6 t = group [
                   rect 160 160
              |> filled optGreen
              |> addOutline (solid 4) black
              ,
                   circle 28
              |> filled optGreen
              |> move (-100,0)
              |> addOutline (solid 4) black
              ,
                  rectangle 27 60
              |> filled optGreen
              |> move (-65,0)
              ,
                    circle 28
              |> filled optGreen
              |> addOutline (solid 4) black
              |> move (0,100)
              ,
                    rectangle 60 25
              |> filled optGreen
              |> move (0, 65)
              ,
              text "Analogizers"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> mirrorX
                |> mirrorY
                |> rotate (degrees 90)
                |> move (10,70)]

              |> move (500,160)
              |> rotate (degrees 90)
              |> move (160-(tranSin (t-200) 500),0)
              |> move (0-(tranSin (t-1050) -500),0)

--botL piece helper
piece7 t = group [
                   rect 160 160
             |> filled optGold
             |> addOutline (solid 4) black
               ,
                   circle 28
             |> filled optGold
             |> move (-100,0)
             |> addOutline (solid 4) black
              ,
                  rectangle 27 60
             |> filled optGold
             |> move (-65,0)
              ,
                    circle 28
             |> filled optGold
             |> addOutline (solid 4) black
             |> move (0,100)
              ,
                    rectangle 60 25
             |> filled optGold
             |> move (0, 65)
             ,
             text "Evolutionaries"
               |> size 22
               |> bold
               |> customFont "Helvetica"
               |> filled white
               |> rotate (degrees 90)
               |> move (0,-70)]

             |> move (-500,-160)
             |> rotate (degrees 90)
             |> mirrorX
             |> mirrorY
             |> move (-160+(tranSin (t-300) 500),0)
             |> move (0+(tranSin (t-1100) -500),0)

--botR piece helper
piece8 t =  group [
                    rect 160 160
              |> filled optPurple
              |> addOutline (solid 4) black
                ,
                    circle 28
              |> filled optPurple
              |> move (-100,0)
              |> addOutline (solid 4) black
               ,
                   rectangle 27 60
              |> filled optPurple
              |> move (-65,0)
               ,
                     circle 28
              |> filled optPurple
              |> addOutline (solid 4) black
              |> move (0,100)
               ,
                     rectangle 60 25
              |> filled optPurple
              |> move (0, 65)
              ,
              text "Connectionists"
                |> size 22
                |> bold
                |> customFont "Helvetica"
                |> filled white
                |> mirrorX
                |> rotate (degrees 90)
                |> move (5,80)]

              |> move (500,-160)
              |> rotate (degrees 90)
              |> mirrorX
              |> move (160-(tranSin (t-400) 500),0)
              |> move (0-(tranSin (t-1150) -500),0)


--masteralgorithm text helper
mA t = group[
              text "Master"
                 |> size 26
                 |> bold
                 |> customFont "Calisto MT"
                 |> filled black
                 |> move (-60, 40)
                 ,
               text "Algorithm"
                 |> size 26
                 |> bold
                 |> customFont "Calisto MT"
                 |> filled black
                 |> move (-60,0)]

                 |> fadeIn t 700
                 |> fadeOut t 900

-- ///////////////////////// HOW MASTER ALGORITHM PROVES USEFUL (SLIDE 9) ///////////////////////// Prepared by Sujan Kandeepan
maUses t = [
             rect 1000 500
             |> filled blue,

             subtitle t "Accurate Answers" -300 50 100,

             checkMark (t-100),

             subtitle t "Faster Decisions" 0 50 200,

             speedBoost (t-200),

             subtitle t "Digital Personal Model" 300 50 300,

             bulb (t-300),

             subtitle t "Society of Models" -300 -200 400,

             people (t-400),

             subtitle t "Machine War" 0 -200 500,

             robot (t-500),

             subtitle t "Replace Humans" 300 -200 600,

             m2c (t-600)
          ]

-- Not in final slide, just placeholders for eaah drawing
tempRect t x y enter = group [
                               rect 100 100
                               |> filled white
                               |> move (x,y)
                               |> fadeIn t enter,

                               text "(Image)"
                               |> size 18
                               |> customFont "Helvetica"
                               |> centered
                               |> filled black
                               |> move (x,y-5)
                               |> fadeIn t enter
                             ]

-- Six subtitles for drawings
subtitle t txt x y enter = group [
                                   text txt
                                   |> size 24
                                   |> customFont "Helvetica"
                                   |> bold
                                   |> centered
                                   |> filled white
                                   |> move (x,y)
                                   |> fadeIn t enter
                                 ]

-- First drawing of check mark
checkMark t = group [
                      -- Shadow
                      drawLine (5*t) (-350,145) (-300,95)
                      |> outlined (solid 25) darkCharcoal,

                      drawLine (5*t-200) (-317.5,95) (-217.5,195)
                      |> outlined (solid 25) darkCharcoal,

                      -- Line
                      drawLine (5*t) (-350,150) (-300,100)
                      |> outlined (solid 25) lightGreen,

                      drawLine (5*t-200) (-317.5,100) (-217.5,200)
                      |> outlined (solid 25) lightGreen
                    ]

-- Second drawing of speed boost symbol
chevron t x = group [
                      -- Behind
                      rect 25 75
                      |> filled yellow
                      |> move (x-5, 165)
                      |> rotate (degrees 45)
                      |> fadeIn t 0,

                      rect 25 50
                      |> filled yellow
                      |> move (x-14, 121)
                      |> rotate (degrees 135)
                      |> fadeIn t 0,

                      -- In front
                      rect 25 75
                      |> filled orange
                      |> move (x, 165)
                      |> rotate (degrees 45)
                      |> fadeIn (3*t) 0,

                      rect 25 50
                      |> filled orange
                      |> move (x-9, 121)
                      |> rotate (degrees 135)
                      |> fadeIn (3*t) 0
                  ]

speedBoost t = group [
                       chevron t -60,

                       chevron (t-25) 0,

                       chevron (t-50) 60
                     ]

-- Third drawing of light bulb
bulb t = group [
                 -- Glass portion
                 circle 40
                 |> filled yellow
                 |> move (300,170),

                 rect 45 40
                 |> filled yellow
                 |> move (300,135),

                 -- Curve down
                 circle 10
                 |> filled blue
                 |> move (275,127.5),

                 circle 10
                 |> filled blue
                 |> move (325,127.5),

                 rect 10 10
                 |> filled blue
                 |> move (280,120),

                 rect 10 10
                 |> filled blue
                 |> move (320,120),

                 -- Base and bottom coil
                 circle 12.5
                 |> filled gray
                 |> move (300,105),

                 rect 25 10
                 |> filled lightGray
                 |> move (300,120),

                 circle 5
                 |> filled lightGray
                 |> move (287.5,120),

                 circle 5
                 |> filled lightGray
                 |> move (312.5,120),

                 rect 25 10
                 |> filled lightGray
                 |> move (300,112.5),

                 circle 5
                 |> filled lightGray
                 |> move (287.5,112.5),

                 circle 5
                 |> filled lightGray
                 |> move (312.5,112.5),

                 rect 25 10
                 |> filled lightGray
                 |> move (300,105),

                 circle 5
                 |> filled lightGray
                 |> move (287.5,105),

                 circle 5
                 |> filled lightGray
                 |> move (312.5,105),

                 -- Fade in, light ray animations
                 rect 100 120
                 |> filled blue
                 |> move (300,150)
                 |> fadeOut t 0,

                 drawLine (3*t-150) (250,170) (225,170)
                 |> outlined (solid 5) yellow,

                 drawLine (3*t-150) (350,170) (375,170)
                 |> outlined (solid 5) yellow,

                 drawLine (3*t-150) (260,200) (240,215)
                 |> outlined (solid 5) yellow,

                 drawLine (3*t-150) (340,200) (360,215)
                 |> outlined (solid 5) yellow,

                 drawLine (3*t-150) (260,140) (240,125)
                 |> outlined (solid 5) yellow,

                 drawLine (3*t-150) (340,140) (360,125)
                 |> outlined (solid 5) yellow
               ]

-- Fourth drawing of people chain made of paper
person t x colour = group [
                            -- Head
                            circle 15
                            |> filled colour
                            |> move (x-300,-80),

                            rect 15 40
                            |> filled colour
                            |> move (x-300,-110),

                            rect 10 30
                            |> filled colour
                            |> move (x-312.5,-102.5)
                            |> rotate (degrees -45),

                            rect 10 30
                            |> filled colour
                            |> move (x-287.5,-102.5)
                            |> rotate (degrees 45),

                            rect 12.5 25
                            |> filled colour
                            |> move (x-307.5,-137.5)
                            |> rotate (degrees -30),

                            rect 12.5 25
                            |> filled colour
                            |> move (x-292.5,-137.5)
                            |> rotate (degrees 30),

                            -- Fade in
                            rect 55 110
                            |> filled blue
                            |> move (x-300,-100)
                            |> fadeOut (1.5*t) 0
                          ]

people t = group [
                   -- Shadows
                   person (t-50) -80 darkCharcoal,

                   person (t-50) 80 darkCharcoal,

                   person (t-25) -40 darkCharcoal,

                   person (t-25) 40 darkCharcoal,

                   person t 0 darkCharcoal,

                   -- People
                   person (t-60) -80 gray
                   |> move (5,5),

                   person (t-60) 80 gray
                   |> move (5,5),

                   person (t-30) -40 gray
                   |> move (5,5),

                   person (t-30) 40 gray
                   |> move (5,5),

                   person t 0 gray
                   |> move (5,5)
                 ]

-- Fifth drawing of robot
robot t = group [
                  -- Head
                  rect 80 80
                  |> filled charcoal
                  |> move (0,-110)
                  |> fadeIn t 0,

                  rect 15 15
                  |> filled darkCharcoal
                  |> move (-20,-62.5)
                  |> fadeIn t 0,

                  rect 15 15
                  |> filled darkCharcoal
                  |> move (20,-62.5)
                  |> fadeIn t 0,

                  -- Eyes
                  circle 15
                  |> filled gray
                  |> move (-20,-100)
                  |> fadeIn t 0,

                  circle 15
                  |> filled gray
                  |> move (20,-100)
                  |> fadeIn t 0,

                  circle 15
                  |> outlined (solid 2) darkCharcoal
                  |> move (-20,-100)
                  |> fadeIn t 0,

                  circle 15
                  |> outlined (solid 2) darkCharcoal
                  |> move (20,-100)
                  |> fadeIn t 0,

                  circle 5
                  |> filled black
                  |> move (-20,-100)
                  |> fadeIn t 0,

                  circle 5
                  |> filled black
                  |> move (20,-100)
                  |> fadeIn t 0,

                  circle 5
                  |> filled red
                  |> move (-20,-100)
                  |> fadeIn (5*t) 250,

                  circle 5
                  |> filled red
                  |> move (20,-100)
                  |> fadeIn (5*t) 250,

                  -- Mouth
                  rect 40 10
                  |> filled gray
                  |> move (0,-130)
                  |> fadeIn t 0,

                  rect 40 2
                  |> filled darkCharcoal
                  |> move (0,-125)
                  |> fadeIn t 0,

                  rect 40 2
                  |> filled darkCharcoal
                  |> move (0,-130)
                  |> fadeIn t 0,

                  rect 40 2
                  |> filled darkCharcoal
                  |> move (0,-135)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (-20,-130)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (-15,-130)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (-5,-130)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (5,-130)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (15,-130)
                  |> fadeIn t 0,

                  rect 2 10
                  |> filled darkCharcoal
                  |> move (20,-130)
                  |> fadeIn t 0
                ]

-- Sixth image of computer replacing man
man t = group [
                -- Head
                circle 15
                |> filled black
                |> move (225,-55),

                -- Shoulders
                rect 40 15
                |> filled black
                |> move (225,-77.5),

                circle 7.5
                |> filled black
                |> move (204,-77.5),

                circle 7.5
                |> filled black
                |> move (246,-77.5),

                -- Arms
                rect 12 50
                |> filled black
                |> move (202.5,-102.5),

                rect 12 50
                |> filled black
                |> move (247.5,-102.5),

                circle 6
                |> filled black
                |> move (202.5,-127.5),

                circle 6
                |> filled black
                |> move (247.5,-127.5),

                -- Torso
                rect 30 50
                |> filled black
                |> move (225,-102.5),

                -- Legs
                rect 14 40
                |> filled black
                |> move (217,-147),

                rect 14 40
                |> filled black
                |> move (233,-147),

                circle 7
                |> filled black
                |> move (217,-167),

                circle 7
                |> filled black
                |> move (233,-167),

                -- Fade in
                rect 75 150
                |> filled blue
                |> move (225,-100)
                |> fadeOut t 0
              ]

arrow t = group [
                  -- Shadow
                  rect 30 20
                  |> filled darkCharcoal
                  |> move (277.5,-102.5)
                  |> fadeIn t 0,

                  triangle 25
                  |> filled darkCharcoal
                  |> move (305,-102.5)
                  |> fadeIn t 0,

                  -- Line
                  rect 30 20
                  |> filled green
                  |> move (280,-100)
                  |> fadeIn t 0,

                  triangle 25
                  |> filled green
                  |> move (307.5,-100)
                  |> fadeIn t 0
                ]

computer t = group [
                     -- Sideways desktop
                     rect 70 35
                     |> filled darkGray
                     |> move (375,-125)
                     |> fadeIn t 0,

                     circle 5
                     |> filled charcoal
                     |> move (350,-120)
                     |> fadeIn t 0,

                     rect 20 2
                     |> filled black
                     |> move (357.5,-132.5)
                     |> fadeIn t 0,

                     rect 25 15
                     |> filled charcoal
                     |> move (390,-125)
                     |> fadeIn t 0,

                     -- Monitor
                     rect 60 45
                     |> filled darkGray
                     |> move (375,-80)
                     |> fadeIn t 0,

                     rect 50 37.5
                     |> filled black
                     |> move (375,-80)
                     |> fadeIn t 0,

                     rect 15 5
                     |> filled darkCharcoal
                     |> move (375,-105)
                     |> fadeIn t 0
                   ]

m2c t = group [
                man t,

                arrow (t-50),

                computer (t-100)
              ]

-- ///////////////////////// MILESTONE IN LEARNING HISTORY ///////////////////////// Prepared by Tom Xu
milestone1 t = [ rect 2000 2000 -- milestone in cs slide 1
                |> filled darkCharcoal,

            computer2
                |> move (-200 + 0.5*t,50)
                |> fadeOut t 300,

            computer2
                |> move (200 - 0.5*t,50)
                |> fadeOut t 300,

            computer2
                |> fadeIn t 370
                |> move (0, 50),

            arrow2
                |> move (-220,50)
                |> fadeIn t 450,

            arrow2
                |> move (350,50)
                |> fadeIn t 450,

             text "Any Type of Data"
                 |> bold
                 |> size 25
                 |> filled darkCharcoal
                 |> move (-382,42)
                 |> fadeIn t 450,

            text "Knowledge"
                 |> bold
                 |> size 25
                 |> filled darkCharcoal
                 |> move (200,42)
                 |> fadeIn t 450,

            text "Algorithms"
                 |> bold
                 |> size 40
                 |> filled grey
                 |> move (-100,180)
                 |> fadeOut t 300,

             text "Master Algorithm"
                 |> bold
                 |> size 40
                 |> filled grey
                 |> move (-150,180)
                 |> fadeIn t 450,


            text "Game of Thrones"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (-280 + 0.5*t,0)
                 |> fadeOut t 200,

            text "Stranger Things"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (-280 + 0.5*t,40)
                 |> fadeOut t 200,

            text "Jeopardy"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (-280 + 0.5*t,80)
                 |> fadeOut t 200,

            text "12 68 75 234 09 11"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (120 - 0.5*t,0)
                 |> fadeOut t 200,

            text "8.9 -7 77 12 776 30"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (120 - 0.5*t,40)
                 |> fadeOut t 200,

            text "98 56 43 232 7 366"
                 |> bold
                 |> size 20
                 |> filled darkCharcoal
                 |> move (120 - 0.5*t,80)
                 |> fadeOut t 200
                ]


milestone2 t =  [ rect 2000 2000
                |> filled black,

            barchart t [("Algorithm 1",1.3),
                         ("Algorithm 2",0.69),
                         ("Algorithm 3",0.57),
                         ("Master Algorithm",1000)]
                         (0,-100)
                 |> move (-300,-50),

            text "Capability of Algorithms"
                 |> size 40
                 |> bold
                 |> filled white
                 |> move (-240, 150),

            rect 2 300
                 |> filled white
                 |> move (-350,0),

            rect 700 2
                 |> filled white
                 |> move (0,-150),

            text "Algorithm"
                 |> size 30
                 |> filled white
                 |> move (-50,-230),

            text "Capability (Arbitrary Scale)"
                 |> size 30
                 |> filled white
                 |> move (-380,-150)
                 |> rotate (3*pi/2)

                ]

computer2 = group [roundedRect 200 150 10
                                       |> filled grey,

                                  roundedRect 200 150 10
                                       |> outlined (solid 5) darkGrey,

                                  rect 20 50
                                       |> filled darkGrey
                                       |> move (0,-100),

                                 rect 100 20
                                       |> filled darkGrey
                                       |> move (0, -120)
                                       ]

arrow2 = group [triangle 50
                                   |> filled grey,

                                rect 155 50
                                   |> filled grey
                                   |> move (-102.1,0)
                                ]

barchart t data (x,y) = group (createBar t data (x,y))

createBar t data (x,y) = case data of
                              (label,h) :: xs -> (group [rect 50 h
                                                |> filled white
                                                |> fadeIn t 100
                                                |> move (x,y + h/2)
                                                ,
                                                text label
                                                |> size 20
                                                |> filled white
                                                |> fadeIn t 100
                                                |> move (x - 55, y - 40)

                                                ]) :: createBar (t - 50) xs (x + 200,y)
                              _ -> []

-- ///////////////////////// ENDING SLIDE ///////////////////////// Prepared by Sujan Kandeepan
thankYou t = [
               rect 1000 500
               |> filled blue,

               text "Thank you for listening!"
               |> size 72
               |> customFont "Helvetica"
               |> bold
               |> centered
               |> filled white
               |> fadeIn t 100,

               text "Feel free to ask questions if you have any."
               |> size 36
               |> customFont "Helvetica"
               |> bold
               |> centered
               |> filled darkGray
               |> move (0,-50)
               |> fadeIn t 200
             ]
