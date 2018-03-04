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

             in collage 1500 750 (slide t ++ borders ++ navigators)

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
              |> move (4000,0),
           rect 5000 5000
              |> filled white
              |> move (-4000,0),
           rect 5000 5000
              |> filled white
              |> move (0,3750),
           rect 5000 5000
              |> filled white
              |> move (0,-3750)]

navigators = [ group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                      ] |> move (600,-300)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                    ] |> rotate (degrees 180)
                      |> move (-600,-300)
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

slides = Array.fromList [graph]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

graph t = [
            text "TTC 1985-2015 Analysis of Ridership (System Totals Only)"
            |> size 36
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled black
            |> move (0,325)
            |> fadeIn t 0,

            drawLine (t-100) (-450,-275) (500,-275) |> outlined (solid 5) black,

            drawLine (t-100) (-450,-277.5) (-450,300) |> outlined (solid 5) black,

            text "Year"
            |> size 30
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled charcoal
            |> move (0,-360)
            |> fadeIn t 250,

            text "Rider Fare Paid"
            |> size 30
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled charcoal
            |> move (-550,0)
            |> rotate (degrees 270)
            |> fadeIn t 250,

            year t 30 1985, year t 60 1986, year t 90 1987, year t 120 1988, year t 150 1989,
            year t 180 1990, year t 210 1991, year t 240 1992, year t 270 1993, year t 300 1994,
            year t 330 1995, year t 360 1996, year t 390 1997, year t 420 1998, year t 450 1999,
            year t 480 2000, year t 510 2001, year t 540 2002, year t 570 2003, year t 600 2004,
            year t 630 2005, year t 660 2006, year t 690 2007, year t 720 2008, year t 750 2009,
            year t 780 2010, year t 810 2011, year t 840 2012, year t 870 2013, year t 900 2014,
            year t 930 2015,

            fare t 47.5 0 0, fare t 7.5 50 50000, fare t -2.5 100 100000, fare t -2.5 150 150000,
            fare t -2.5 200 200000, fare t -2.5 250 250000, fare t -2.5 300 300000, fare t -2.5 350 350000,
            fare t -2.5 400 400000, fare t -2.5 450 450000, fare t -2.5 500 500000, fare t -2.5 550 550000,

            bar t 30 432.160 450, bar t 60 441.012 454, bar t 90 456.884 458, bar t 120 463.475 462,
            bar t 150 450.726 466, bar t 180 459.234 470, bar t 210 424.167 474, bar t 240 404.251 478,
            bar t 270 393.485 482, bar t 300 388.252 486, bar t 330 388.152 490, bar t 360 372.430 494,
            bar t 390 379.883 498, bar t 420 388.689 502, bar t 450 392.593 506, bar t 480 410.558 510,
            bar t 510 419.993 514, bar t 540 415.539 518, bar t 570 405.412 522, bar t 600 418.099 526,
            bar t 630 431.220 530, bar t 660 444.544 534, bar t 690 459.769 538, bar t 720 466.700 542,
            bar t 750 471.233 546, bar t 780 477.357 550, bar t 810 500.219 554, bar t 840 514.007 558,
            bar t 870 525.194 562, bar t 900 534.815 566, bar t 930 534.005 560
          ]

year t x yr = group [
                   text ("-" ++ toString yr)
                   |> size 18
                   |> customFont "Helvetica"
                   |> bold
                   |> filled lightCharcoal
                   |> move (x-460,-275)
                   |> rotate (degrees 90)
                   |> fadeIn t 350
                 ]

fare t x y fr = group [
                      text ("$" ++ toString fr ++ "-")
                      |> size 18
                      |> customFont "Helvetica"
                      |> bold
                      |> filled lightCharcoal
                      |> move (x-525,y-275)
                      |> fadeIn t 350
                 ]

bar t x y enter = group [
                    drawLine (t-enter) (x-455,-272.5) (x-455,y-275)
                    |> outlined (solid 25) lightRed
                  ]
