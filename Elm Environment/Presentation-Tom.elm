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

             in collage 1000 500 (slide t ++ navigators)

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

slides = Array.fromList [slide1, slide2, slide3, slide4]

slide1 t = [ -- Connectionist
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

slide2 t = [ roundedRect 50 50 5 --connectionists slide 2
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

slide3 t = [ rect 2000 2000 -- milestone in cs slide 1
                |> filled darkCharcoal,

            computer
                |> move (-200 + 0.5*t,50)
                |> fadeOut t 300,

            computer
                |> move (200 - 0.5*t,50)
                |> fadeOut t 300,

            computer
                |> fadeIn t 370
                |> move (0, 50),

            arrow
                |> move (-220,50)
                |> fadeIn t 450,

            arrow
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


slide4 t =  [ rect 2000 2000
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


computer = group [roundedRect 200 150 10
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

arrow = group [triangle 50
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
