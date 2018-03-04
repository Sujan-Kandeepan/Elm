module Main exposing (..)

import GraphicSVG exposing (..)
import Array


type Message
    = GameTick Float GetKeyState
      --The tick needs to have Float and GetKeyState which handles key presses.
    | NextSlide
    | LastSlide



-- this is the main function, and for simple animations, you would only replace the view function, or edit it below


main =
    gameApp GameTick
        { model = init
        , view = view
        , update = update
        }



-- MODEL


init =
    { t = 0
    , idx = 0
    , p = False
    , -- Pause
      r = 1
    , -- Rewind
      a =
        1
        -- Acceleration
    }



-- VIEW


view model =
    let
        t =
            model.t

        slide =
            Maybe.withDefault default (Array.get model.idx slides)
    in
        collage 1000 500 (slide t ++ borders ++ navigators)



-- UPDATE


update message model =
    case message of
        GameTick tick ( getKeyState, changeP1, changeP2 ) ->
            if (getKeyState LeftArrow) == JustDown then
                { model
                    | t = 0
                    , idx = max (model.idx - 1) 0
                }
            else if (getKeyState RightArrow) == JustDown then
                { model
                    | t = 0
                    , idx = min (model.idx + 1) (Array.length slides - 1)
                }
            else if (getKeyState Space) == JustDown then
                { model
                    | p = not model.p
                }
            else if (getKeyState UpArrow) == JustDown then
                { model
                    | a = min (model.a * 2) 4
                }
            else if (getKeyState DownArrow) == JustDown then
                { model
                    | a = max (model.a / 2) 0.5
                }
            else if (getKeyState (Key "R")) == JustDown then
                { model
                    | r = -model.r
                }
            else if (getKeyState Backspace) == JustDown then
                { model
                    | t = 0
                }
            else if model.p then
                model
            else
                { model
                    | t = max (model.t + 2.5 * model.a * model.r) 0
                }

        NextSlide ->
            { model
                | t = 0
                , idx = min (model.idx + 1) (Array.length slides - 1)
            }

        LastSlide ->
            { model
                | t = 0
                , idx = max (model.idx - 1) 0
            }



--- MISCELLANEOUS


default t =
    []


borders =
    [ rect 5000 5000
        |> filled white
        |> move ( 3000, 0 )
    , rect 5000 5000
        |> filled white
        |> move ( -3000, 0 )
    , rect 5000 5000
        |> filled white
        |> move ( 0, 2750 )
    , rect 5000 5000
        |> filled white
        |> move ( 0, -2750 )
    ]


navigators =
    [ group
        [ circle 40
            |> filled gray
        , triangle 30
            |> filled white
        ]
        |> move ( 450, -200 )
        |> makeTransparent 0.5
        |> notifyTap NextSlide
    , group
        [ circle 40
            |> filled gray
        , triangle 30
            |> filled white
        ]
        |> rotate (degrees 180)
        |> move ( -450, -200 )
        |> makeTransparent 0.5
        |> notifyTap LastSlide
    ]



-- FUNCTIONS
--<< So why do I see (t - 100) or whatever value so often? >>
--   Whenever I do that, I'm basically delaying what I want to happen
--   by that value. Is it measure in seconds, frames or what? What's the unit here?
--   To be honest, I don't know. It has a lot to do with the UPDATE function, and
--   what value for 'x' you are using for " t = model.t + x ".


disappear x n =
    if x > n then
        makeTransparent 0
    else
        makeTransparent 1



-- Makes things vanish off the screen!


loop t n =
    let
        y =
            toFloat (floor (t / n))

        -- This function is how I make things loop!
    in
        t - y * n


appear x n =
    if x > n then
        makeTransparent 1
    else
        makeTransparent 0



-- Makes things suddenly appear on the screen!


fadeIn t n =
    makeTransparent (tranSin (t - n) 1)


fadeOut t n =
    makeTransparent (1 - (tranSin (t - n) 1))


trans t y =
    if
        t < 0
        -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
    then
        0
    else
        Basics.min t y


tranSin t y =
    if
        t < 0
        -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
    then
        0
    else if t / 100 > pi / 2 then
        y
    else
        sin (t / 100) * y


drawLine t ( x1, y1 ) ( x2, y2 ) =
    line ( x1, y1 ) ( x1 + tranSin (t) (x2 - x1), y1 + tranSin (t) (y2 - y1) )



-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.


slides =
    Array.fromList [ slide1, slide2, slide3, slide4 ]



--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-


slide1 t =
    [ ngon 8 200
        |> outlined (solid 2) lightCharcoal
        |> move ( -200, 0 )
        |> rotate (degrees 100 * cos (t / 100))
    , ngon 6 200
        |> outlined (solid 2) gray
        |> move ( 200, 0 )
        |> rotate (degrees 100 * cos (t / 100))
    , text "Wall Street : The first domino to fall"
        |> size 45
        |> customFont "Helvetica"
        |> bold
        |> centered
        |> outlined (solid 2) brown
        |> move ( 0, 25 )
    , text "The Evolution of the Financial Industry"
        |> size 30
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> move ( 0, -40 )
        |> fadeIn t 600
    , bent t
        |> move ( -35, 75 )
    , rect 1000 500
        |> filled green
        |> move ( 500 + (tranSin (t - 400) 510), 0 )
    , rect 1000 500
        |> filled lightGreen
        |> move ( -500 - (tranSin (t - 400) 510), 0 )
    , text "BROUGHT TO YOU"
        |> size 50
        |> customFont "Helvetica"
        |> bold
        |> filled white
        |> move ( -480, 190 )
        |> appear t 100
        |> disappear t 350
    , text "BY WALL STREET"
        |> size 50
        |> customFont "Helvetica"
        |> bold
        |> filled white
        |> move ( 40, -150 )
        |> appear t 200
        |> disappear t 350
    , dollar t
        |> appear t 0
        |> disappear t 400
    , dollarTwo t
        |> appear t 0
        |> disappear t 400
    , dollarThree t
        |> appear t 0
        |> disappear t 400
    , dollarFour t
        |> appear t 0
        |> disappear t 400
    , dollarFive t
        |> appear t 200
        |> disappear t 450
    , dollarSix t
        |> appear t 200
        |> disappear t 450
    ]


slide2 t =
    [ rect 1500 1500
        |> filled gray
        |> move ( 0, 0 )
    , rect 700 40
        |> filled brown
        |> move ( 0, -90 )
    , rect 20 200
        |> filled brown
        |> move ( -190, -200 )
    , rect 20 200
        |> filled brown
        |> move ( 190, -200 )
    , rect 700 200
        |> filled lightCharcoal
        |> move ( 0, 100 )
    , text "0s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -100 * cos (t / 300), -100 * sin (t / 300) )
        |> fadeIn t 600
    , text "1s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -150 * cos (t / 300), -150 * sin (t / 300) )
        |> fadeIn t 600
    , text "0s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -125 * cos (t / 200), -125 * sin (t / 200) )
        |> fadeIn t 600
    , text "1s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -175 * cos (t / 200), -175 * sin (t / 200) )
        |> fadeIn t 600
    , text "0s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -125 * cos (t / 100), -125 * sin (t / 100) )
        |> fadeIn t 600
    , text "1s"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled lightGreen
        |> scale 0.75
        |> move ( -175 * cos (t / 100), -175 * sin (t / 100) )
        |> fadeIn t 600
    , text "Thomas Peterffy: The Man Who Started The Revolution"
        |> size 24
        |> customFont "Helvetica"
        |> bold
        |> centered
        |> filled darkCharcoal
        |> move ( -20, 220 )
        |> fadeIn t 100
    , group
        (makeBullets (t - 100)
            [ "A computer programmer who worked as a trader on Wall Street"
            , "Had a background in Civil Engineering"
            , "A self-taught coder who built algorithms for trading shares"
            , "Would later become a man worth more than $5 billion"
            ]
            0
        )
        |> move ( -20, 170 )
    , laptop t
        |> appear t 500
        |> scale 0.65
        |> move ( 100, 30 )
    , computer t
        |> appear t 550
        |> move ( 180, -40 )
        |> rotate (degrees 90)
        |> scale 0.3
    , another t
        |> appear t 550
        |> move ( 60, -40 )
        |> rotate (degrees 90)
        |> scale 0.3
    ]


slide3 t =
    [ rect 1500 1500
        |> filled lightBlue
        |> move ( 0, 0 )
    , cloud t |> appear t 200 |> scale 1 |> move ( -280, -200 )
    , cloudTwo t |> appear t 200 |> scale 1 |> move ( 460, -200 )
    , ngon 8 400
        |> filled lightYellow
        |> rotate (degrees 100 * cos (t / 80))
    , ngon 8 150
        |> filled yellow
        |> rotate (degrees 100 * cos (t / 80))
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 300), -200 * sin (t / 300) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 200), -200 * sin (t / 200) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 100), -200 * sin (t / 100) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 150), -200 * sin (t / 150) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 250), -200 * sin (t / 250) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -200 * cos (t / 175), -200 * sin (t / 175) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -200 * cos (t / 100), -200 * sin (t / 100) )
        |> fadeIn t 600
    , text "$"
        |> size 35
        |> customFont "Helvetica"
        |> centered
        |> filled green
        |> scale 0.95
        |> move ( -300 * cos (t / 230), -200 * sin (t / 230) )
        |> fadeIn t 600
    , text "Late 1970's : The Dawn of Programmers on Wall Street"
        |> size 24
        |> customFont "Helvetica"
        |> bold
        |> centered
        |> filled darkCharcoal
        |> move ( -20, 200 )
        |> fadeIn t 100
    , group
        (makeBullets (t - 100)
            [ "Recruiters from the financial sector successfuly recruited young talent"
            , "New programmers were helping firms put trading algorithms into code"
            , "As algorithms grew more complicated, better coders were needed"
            ]
            0
        )
        |> move ( -100, 140 )
    , rect 120 35
        |> filled black
        |> move ( -430, 170 )
        |> scale 1.2
    , rect 15 400
        |> filled gray
        |> move ( -495, 0 )
    , text "Wall Street"
        |> size 18
        |> bold
        |> customFont "Helvetica"
        |> centered
        |> filled white
        |> move ( -430, 160 )
    , people t |> fadeIn t 1000 |> scale 0.75 |> move ( 0, -80 )
    ]


slide4 t =
    [ rect 1500 1500
        |> filled darkCharcoal
        |> move ( 0, 0 )
    , circle
        410
        |> filled gray
        |> move
            ( 100 * cos (t / 100)
            , -100 * sin (t / 100)
            )
    , rect 50 180
        |> filled darkBlue
        |> move ( -400, -250 )
    , rect 50 110
        |> filled blue
        |> move ( -350, -250 )
    , rect 50 130
        |> filled blue
        |> move ( -450, -250 )
    , rect 50 180
        |> filled darkBlue
        |> move ( -300, -250 )
    , rect 50 180
        |> filled darkBlue
        |> move ( 400, -250 )
    , rect 50 110
        |> filled blue
        |> move ( 450, -250 )
    , rect 50 130
        |> filled blue
        |> move ( 350, -250 )
    , rect 50 180
        |> filled darkBlue
        |> move ( 300, -250 )
    , rect 50 180
        |> filled blue
        |> move ( 250, -260 )
    , rect 50 180
        |> filled blue
        |> move ( -250, -260 )
    , rect 50 180
        |> filled darkBlue
        |> move ( 200, -210 )
    , rect 50 180
        |> filled darkBlue
        |> move ( -200, -210 )
    , rect 50 180
        |> filled blue
        |> move ( 150, -240 )
    , rect 50 180
        |> filled blue
        |> move ( -150, -240 )
    , text "The Current State of Wall Street"
        |> size 24
        |> customFont "Helvetica"
        |> bold
        |> centered
        |> filled black
        |> move ( -70, 160 )
        |> fadeIn t 100
    , group
        (makeBullets (t - 100)
            [ "New algorithms are being formulated daily on Wall Street "
            , "The virtual world of algorithm represents the current state of global financial sector"
            , "Humans have largely been reduced to interested observers"
            ]
            0
        )
        |> move ( -150, 100 )
    , tablet t |> appear t 600 |> move ( 0, -100 ) |> scale 0.5
    , really t |> fadeIn t 2200
    ]


butHonestly t =
    group
        [ rect 1200 600
            |> filled darkGreen
        , makeItRain (t - 1000)
        , makeItRain (1000 - t)
        , text "THE ALGORITHMS OWN THE MARKET NOW!"
            |> size 40
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled white
            |> move ( 0, -20 )
            |> fadeIn t 1100
        ]


makeBill t n =
    let
        x =
            (n * 2) * cos (degrees (n * 10))

        y =
            (n * 2) * sin (degrees (n * 10))
    in
        group
            [ text "0 1"
                |> size 36
                |> customFont "Helvetica"
                |> bold
                |> centered
                |> filled billGreen
                |> move ( x, y )
                |> fadeIn (loop (t - n * 5) 300) 0
                |> fadeOut (loop (t - n * 5 + 100) 300) 0
            ]


billGreen =
    rgb 133 187 101


makeItRain t =
    group (List.map (makeBill t) [0..300])


dollar t =
    group
        [ text "$"
            |> size 100
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( 0, -100 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


dollarTwo t =
    group
        [ text "$"
            |> size 100
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( 0, -200 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


dollarThree t =
    group
        [ text "$"
            |> size 100
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( 0, 0 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


dollarFour t =
    group
        [ text "$"
            |> size 100
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( 0, 100 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


dollarFive t =
    group
        [ text "$"
            |> size 200
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( -300, -50 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


dollarSix t =
    group
        [ text "$"
            |> size 200
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled darkGreen
            |> move ( 300, -50 )
            |> fadeIn t 100
            |> fadeOut t 400
        ]


binary t n =
    let
        x =
            (n * 2) * cos (degrees (n * 50))

        y =
            (n * 2) * sin (degrees (n * 50))
    in
        group
            [ text "0 1"
                |> size 50
                |> customFont "Helvetica"
                |> bold
                |> centered
                |> filled lightGreen
                |> move ( x, y )
                |> fadeIn (loop (t - n * 5) 300) 0
                |> fadeOut (loop (t - n * 5 + 100) 300) 0
            ]


numbers t =
    group (List.map (binary t) [0..300])


really t =
    group
        [ rect 1200 600
            |> filled white
        , numbers (t - 1000)
        , numbers (1000 - t)
        , text "THE ALGORITHMS OWN THE MARKET NOW!"
            |> size 40
            |> customFont "Helvetica"
            |> bold
            |> centered
            |> filled black
            |> move ( 0, -20 )
            |> fadeIn t 800
        ]


phone t =
    group
        [ filled black (rect 100 50) |> scale 4 |> rotate (degrees 270)
        , filled charcoal (circle 15) |> move ( 0, -160 )
        , filled charcoal (oval 20 80) |> rotate (degrees 90) |> scale 0.75 |> move ( 0, 175 )
        , filled blue (circle 5) |> move ( -50, 175 )
        , outlined (solid 3) yellow (rect 101 51) |> scale 4 |> rotate (degrees 270)
        , filled darkCharcoal (rect 47 75) |> scale 4 |> rotate (degrees 180) |> move ( 0, 11 )
        ]


another t =
    group
        [ filled black (rect 100 50) |> scale 4 |> rotate (degrees 270)
        , filled charcoal (circle 15) |> move ( 0, -160 )
        , filled charcoal (oval 20 80) |> rotate (degrees 90) |> scale 0.75 |> move ( 0, 175 )
        , outlined (solid 3) gray (rect 101 51) |> scale 4 |> rotate (degrees 270)
        , filled darkCharcoal (rect 47 75) |> scale 4 |> rotate (degrees 180) |> move ( 0, 11 )
        ]


computer t =
    group
        [ filled black (rect 100 50) |> scale 4 |> rotate (degrees 270)
        , filled charcoal (circle 15) |> move ( 0, -160 )
        , filled charcoal (oval 20 80) |> rotate (degrees 90) |> scale 0.75 |> move ( 0, 175 )
        , filled blue (circle 5) |> move ( -50, 175 )
        , outlined (solid 3) gray (rect 101 51) |> scale 4 |> rotate (degrees 270)
        , filled darkCharcoal (rect 47 75) |> scale 4 |> rotate (degrees 180) |> move ( 0, 11 )
        ]


tablet t =
    group
        [ filled black (rect 102 50) |> scale 4 |> rotate (degrees 270)
        , filled blue (circle 5) |> move ( -50, 175 )
        , outlined (solid 1) gray (rect 103 51) |> scale 4 |> rotate (degrees 270)
        , filled darkCharcoal (rect 47 85) |> scale 4 |> rotate (degrees 180) |> move ( 0, -10 )
        , text "1010101011001"
            |> size 25
            |> customFont "Helvetica"
            |> centered
            |> filled lightGreen
            |> scale 0.75
            |> move ( 10 * cos (t / 200), -10 * sin (t / 200) )
        ]


laptop t =
    group
        [ filled gray (rect 100 100) |> scale 1.7 |> move ( -375, -50 )
        , filled charcoal (rect 100 50) |> scale 1.7 |> move ( -375, -125 )
        , outlined (solid 2) darkCharcoal (rect 105 72) |> scale 1.6 |> move ( -375, -25 )
        , filled darkCharcoal (rect 25 35) |> scale 1.5 |> move ( -320, -125 )
        , filled darkCharcoal (rect 70 35) |> scale 1.5 |> move ( -400, -125 )
        , filled blue (rect 80 40) |> scale 1.7 |> move ( -375, -20 )
        ]


bent t =
    group
        [ outlined (solid 5) black (openPolygon [ ( 0, 0 ), ( 13, 17 ), ( 15, 32 ), ( 25, 15 ), ( 22, 0 ) ])
        , outlined (solid 5) black (openPolygon [ ( 15, 32 ), ( 17, 60 ), ( 19, 65 ) ])
        , outlined (solid 5) black (openPolygon [ ( 19, 65 ), ( 7, 56 ), ( 3, 45 ) ]) |> move ( 0, -8 )
        , outlined (solid 5) black (openPolygon [ ( 19, 65 ), ( 25, 50 ), ( 35, 45 ) ]) |> move ( 0, -8 )
        , filled black (circle 10) |> move ( 20, 70 ) |> move ( 0, -8 )
        , outlined (solid 3) lightCharcoal (circle 50) |> scale 0.15 |> move ( 40, 80 )
        , outlined (solid 3) lightCharcoal (circle 50) |> scale 0.3 |> move ( 55, 100 )
        , outlined (solid 3) lightCharcoal (circle 50) |> scale 0.6 |> move ( 100, 130 )
        ]
        |> move ( 20, 0 )


cloud t =
    group
        [ filled gray (circle 120)
            |> move ( -100, 0 )
        , filled
            gray
            (circle 75)
            |> move ( -70, 100 )
        , filled
            gray
            (circle 45)
            |> move ( -40, 10 )
        ]


cloudTwo t =
    group
        [ filled gray (circle 120)
            |> move ( -100, 0 )
        , filled
            gray
            (circle 75)
            |> move ( -70, 100 )
        , filled
            gray
            (circle 45)
            |> move ( -40, 10 )
        ]


people t =
    group
        [ filled brown (rect 140 10) |> scale 3 |> move ( 0, 0 )
        , filled darkCharcoal (circle 25) |> move ( 0, 150 )
        , filled darkCharcoal (oval 50 100) |> rotate (degrees 0) |> scale 0.75 |> move ( 0, 100 )
        , filled darkCharcoal (rect 10 80) |> scale 1 |> move ( -10, 50 )
        , filled darkCharcoal (rect 10 80) |> scale 1 |> move ( 10, 50 )
        , filled darkCharcoal (rect 80 10) |> scale 0.9 |> move ( 40, 120 ) |> rotate (degrees 25)
        , filled darkCharcoal (rect 80 10) |> scale 0.9 |> move ( -40, 120 ) |> rotate (degrees -25)
        , filled black (circle 22) |> move ( 0, -50 )
        , filled black (circle 22) |> move ( 100, -30 )
        , filled black (circle 22) |> move ( -100, -50 )
        , filled black (circle 22) |> move ( 50, -30 )
        , filled black (circle 22) |> move ( -50, -40 )
        , filled black (circle 22) |> move ( -150, -40 )
        , filled black (circle 22) |> move ( 150, -30 )
        , filled black (circle 22) |> move ( 200, -50 )
        , filled black (circle 22) |> move ( -200, -35 )
        , filled black (circle 22) |> move ( 0, -100 )
        , filled black (circle 22) |> move ( 50, -90 )
        , filled black (circle 22) |> move ( -50, -90 )
        , filled black (circle 22) |> move ( 100, -100 )
        , filled black (circle 22) |> move ( -100, -110 )
        , filled black (circle 22) |> move ( 150, -100 )
        , filled black (circle 22) |> move ( -150, -100 )
        ]



-- This is just for AUTOMATIC bullets. You are free to make
-- more customized bullets, but they will take a bit longer.


makeBullets t l start =
    case l of
        x :: xs ->
            group
                [ text x
                    |> size 20
                    |> customFont "Helvetica"
                    |> filled black
                    |> move ( -200, start )
                    |> fadeIn t 100
                , circle 5
                    |> filled black
                    |> move ( -220, start + 5 )
                    |> fadeIn t 100
                ]
                :: makeBullets (t - 100) xs (start - 35)

        _ ->
            []
