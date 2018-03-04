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

borders =[rect 5000 5000
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
              |> move (0,-2750)
              ]



navigators = [ group [ circle 40
                        |> filled blue
                      ,
                      circle 30
                        |> filled black

                      ] |> move (450,-200)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 40
                        |> filled gray
                      ,
                      circle 30
                        |> filled white

                    ] |> rotate (degrees 180)
                      |> move (-450,-200)
                      |> makeTransparent 0.5
                |> notifyTap LastSlide

                ]



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

slides = Array.fromList [slide1, slide2]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

slide1 t = [ circle 3000
                |> filled green
                |> move (300 * cos(t/100), 300 * sin(t/100)) ,
                 circle 40
                |> filled gray
                |> move (200 * cos(t/80), 150 * sin(t/80)),
                triangle 40
                |> filled red
                |> move (100 * cos(t/80), 100 * sin(t/80)),
                square 60
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


slide2 t = [ triangle 1200
                |> filled darkRed
                |> move (200 * cos(t/80), 100 * sin(t/80)) ,
                text "How Algorthims are used today?"
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
