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
