import GraphicSVG exposing(..)

-- this is the main function
-- it calls gameApp, which takes two inputs
-- Tick - a function, a special function called a Constructor
-- {...} is a record, in a record we have fields with names
-- to change something in a record 
--   {  nameOfTheRecord | nameOfTheField = newValue }
-- to get something out of record use 
--   nameOfRecord.nameOfField
main = gameApp Tick {
                        model = init    -- init is the value in the field model
                    ,   view = view
                    ,   update = update
                    }

-- this is a type, we call it Msg because it is the type of messages we handle
-- Tick, Red and Blue are called constructors or constructor functions
-- each of these messages is a transition, even though they are in a data type
type Msg = Tick Float GetKeyState
         | Red   -- these are the messages we care about
         | Blue
         | Orange
         | Move

-- this defines a record called init, which is the initial state of the game
-- it must contain all the information which parametrizes our state
init = { t = 0          -- it knows the time
       , pos = (0,0)    -- it knows the position
       , clr = yellow   -- it knows a colour
       , slothPosition = HangingByBothArms WasRight
       }

-- examples of states for a player
type Sloth = HangingByRightArm
           | HangingByLeftArm
           | Napping
           | HangingByBothArms LeftOrRight -- remember history
           | ClimbingUp Float
           | ClimbingDown Float -- fraction of the tree from bottom

type LeftOrRight = WasLeft | WasRight

lateralMotion old = case old of
               HangingByRightArm -> HangingByBothArms WasRight
               HangingByBothArms WasRight -> HangingByLeftArm
               HangingByLeftArm -> HangingByBothArms WasLeft
               HangingByBothArms WasLeft -> HangingByRightArm
               otherwise -> otherwise -- a variable always matches

-- this is a function which takes the model (another word for state) and draws a view of it
view model = collage 500 500 [
                               thing model |> move model.pos 
                                           |> scale 2 
                                           |> rotate (degrees 180)
                               -- same as: move model.pos (thing model)
                             , text "move" |> size 20
                                            |> filled black
                                            |> move (-18,-80)
                             , rect 45 20 |> outlined (solid 3) orange
                                          |> move (0,-75)
                                          |> notifyTap Move
                             , text (toString model.slothPosition)
                                            |> filled brown
                                            |> move (-10,-100)
                             ]
-- update takes messages and performs transitions
update msg model = case msg of
    Tick t (getKeyState,p1,p2) -> let (x1,y1) = p1
                                      (x2,y2) = p2
                                  in { model | t = t }
    Red -> let (x,y) = model.pos
           in { model | pos = (x+10,y), clr = red }
    Blue -> let (x,y) = model.pos
            in { model | pos = (x-10,y), clr = blue }
    Orange -> { model | pos = (0,0) }
    Move -> { model | slothPosition = lateralMotion model.slothPosition }

thing model = group [ rect 100 50 |> filled model.clr
                    , circle 20 |> filled red |> addOutline (solid 1) white |> move (-25,0) |> notifyTap Red
                    , circle 20 |> filled blue |> addOutline (solid 1) white|> move (25,0) |> notifyTap Blue
                    ]
