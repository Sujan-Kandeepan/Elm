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

slides = Array.fromList [slide1, slide2]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

slide1 t = [ rect 1500 1500
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
     
slide2 t = [  rect 1500 1500
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
                square 50
                |> filled blue
                |> move (0,-85)
                |> move (700 + (tranSin (t/2) -700),0)
                |> fadeOut t 500,
                 square 50
                |> filled lightBrown
                |> move (0,-25)
                |> move (-700 + (tranSin (t/2) 700),0)
                |> fadeOut t 500,
                square 60
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