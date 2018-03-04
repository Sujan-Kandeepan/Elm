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
              |> filled optBlue
              |> move (3000,0),
           rect 5000 5000
              |> filled optBlue
              |> move (-3000,0),
           rect 5000 5000
              |> filled optBlue
              |> move (0,2750),
           rect 5000 5000
              |> filled optBlue
              |> move (0,-2750)]

navigators = [ group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled gray

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

slides = Array.fromList [slide6, slide8]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-


--slide 6 is Bayesian, by Ryan Nourbaran
slide6 t =  [square 1000
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
              text "Bayesian Tribe"
                |> size 70
                |> bold
                |> customFont "Helvetica"
                |> filled black
                |> move (-200,-180)
                |> fadeIn t 1200

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



--slide8 is the combining of all tribes to form the master algorithm -- by Ryan Nourbaran
slide8 t = [
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
                   square 160
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
                   square 160
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
                   square 160
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
                   square 160
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
                   square 160
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
                   square 160
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
                   square 160
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
                    square 160
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
--COLOURS
optRed = rgb 211 94 95
optBlue = rgb 40 110 250
optGreen = rgb 12 200 11
optOrange = rgb 245 111 66
optPurple = rgb 194 73 197
optGold = rgb 204 194 30
optGray = rgb 128 133 133
