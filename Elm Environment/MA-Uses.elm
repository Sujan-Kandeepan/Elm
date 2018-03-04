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

slides = Array.fromList [slide1]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

slide1 t = [ -- accurate answers, faster decisions, digital personal model, society of models, machine war, replace humans
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

--Six subtitles for drawings
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
