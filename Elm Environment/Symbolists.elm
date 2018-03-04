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

slide1 t = [
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
