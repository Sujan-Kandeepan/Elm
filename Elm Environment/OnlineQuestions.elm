-- Question #1
type Light = Red
           | Yellow
           | Green

-- Question #2
buttons = group [ text "Choose your flavour:"
                    |> size 18
                    |> bold
                    |> centered
                    |> filled black
                    |> move(0,65)
                , circle 50
                    |> filled red
                    |> move (-75,0)
                    |> notifyTap (ChangeFlavour Strawberry)
                , circle 50
                    |> filled yellow
                    |> move (75,0)
                    |> notifyTap (ChangeFlavour Banana)
                ]

-- Question #3
type Flavour = Strawberry
             | Banana
             | Blueberry
             | None

buttons = group [ text "Choose your flavour:"
                    |> size 18
                    |> bold
                    |> centered
                    |> filled black
                    |> move(0,65)
                , circle 50
                    |> filled red
                    |> move (-150,0)
                    |> notifyTap (ChangeFlavour Strawberry)
                , circle 50
                    |> filled yellow
                    |> move (0,0)
                    |> notifyTap (ChangeFlavour Banana)
                , circle 50
                    |> filled blue
                    |> move(150,0)
                    |> notifyTap (ChangeFlavour Blueberry)
                ]

-- Question #4
scavengerHunt clues = case Array.get 0 clues of
                        Just x -> if x == 7 then [0] else [0] ++ scavengerHunt2 x clues
                        nothing -> []

scavengerHunt2 num clues = case Array.get num clues of
                             Just x -> if x == 7 then [num, 7] else [num] ++ scavengerHunt2 x clues
                             nothing -> []

-- Question #5
number : Person -> Int
number p = case p of
            Unknown -> 0
            Known name m f -> 1 + number m + number f

-- Question #6
update msg model = case msg of
                     PigeonArrived -> {model | numPigeons = model.numPigeons + 1}
                     PigeonDeparted -> {model | numPigeons = model.numPigeons - 1}
                     otherwise      -> model

-- Question #7
update msg model = case msg of
                     PigeonArrived -> {model | numPigeons = model.numPigeons + 1}
                     PigeonDeparted -> if (model.numPigeons == 0) then {model | numPigeons = 0}
                                       else {model | numPigeons = model.numPigeons - 1}
                     otherwise      -> model

-- Question #8
sum xs = case xs of
             []        -> 0
             (x::xs) -> x + sum xs

-- Question #9
evenOddSublists xs = case xs of
           [] -> ([],[])
           [x] -> ([x],[])
           (x::y::xs) -> let (even,odd) = evenOddSublists xs in (x::even,y::odd)

-- Question #10
fixChars chars = case chars of
                   (' '::'2'::' '::more) -> (' '::'t'::'o'::fixChars(' '::more))
                   (' '::'4'::' '::more) -> (' '::'f'::'o'::'r'::fixChars(' '::more))
                   (' '::'u'::' '::more) -> (' '::'y'::'o'::'u'::fixChars(' '::more))
                   (x::more) -> (x::fixChars(more))
                   [] -> []

-- Question #11
type Human = Exists Name Age Company
           | DNE

-- Question #12
database: List Human
database = [(Exists "Bob" 34 "Leaf Removal Company"),
            (Exists "Jimmy" 38 "BCS Law Offices Ldt"),
            (Exists "Samantha" 40 "Destiny Space Travel Ldt"),
            (Exists "Carl" 15 "Stay in the House Building Co"),
            (Exists "Kate" 35 "Polar Heating Unldt")]

-- Question #13
countAge: Age -> List Human -> Int
countAge correct humans = case humans of
                        (Exists name age company)::xs -> if age == correct then 1 + countAge correct xs
                                                         else 0 + countAge correct xs
                        DNE::xs -> 0 + countAge correct xs
                        [] -> 0

-- Question #14
maxAge: List Human -> Age
maxAge humans = case humans of
                  ((Exists name1 age1 company1)::(Exists name2 age2 company2)::xs) ->
                    if age1 > age2 then maxAge ((Exists name1 age1 company1)::xs)
                    else maxAge ((Exists name2 age2 company2)::xs)
                  [(Exists name1 age1 company1)] -> age1
                  DNE::xs -> maxAge xs
                  (Exists name1 age1 company1)::DNE::xs -> maxAge ((Exists name1 age1 company1)::xs)
                  [] -> 0

-- Question #15
listEmployees: Company -> List Human -> List Name
listEmployees correct database = case database of
                                  (Exists name age company)::xs ->
                                    if company == correct then [name] ++ listEmployees correct xs
                                    else listEmployees correct xs
                                  DNE::xs -> listEmployees correct xs
                                  [] -> []

-- Question #17
escapeVelocity: Float -> Float -> Float
escapeVelocity mass radius = sqrt(2*g*mass/radius)

-- Question #18
update msg model = case msg of
                     Tick t _ ->
                       let dt      = t - model.time
                           (px,py) = model.pos
                           (vx,vy) = model.vel
                       in  { model | time = t
                                   , pos = (px + dt*vx,py + dt*vy)
                                   , vel = (vx ,vy + dt*gravity)
                           }

-- Question #20
rotate xs = case xs of
            [] -> []
            (x::xs) -> xs ++ [x]

-- Question #21
merge (xs,ys) = case (xs,ys) of
                  (x::xx,y::yy) -> if x < y then (x::merge(xx,ys))
                                   else (y::merge(xs,yy))
                  (x::xx,[]) -> (x::merge(xx,[]))
                  ([],y::yy) -> (y::merge([],yy))
                  ([],[]) -> []

-- Question #22
mergeSort xs = List.sort xs

-- Question #24
fixChars chars = case chars of
                   ('.'::' '::x::more) -> ('.'::' '::Char.toUpper x::fixChars more)
                   ('?'::' '::x::more) -> ('?'::' '::Char.toUpper x::fixChars more)
                   ('!'::' '::x::more) -> ('!'::' '::Char.toUpper x::fixChars more)
                   (x::more) -> (x::fixChars(more))
                   [] -> []

-- Question #25
advanceTime: TimeOfDay -> TimeOfDay
advanceTime time =
  case time of
    Morning -> LateMorning
    LateMorning -> Noon
    Noon -> Afternoon
    Afternoon -> LateAfternoon
    LateAfternoon -> Evening
    Evening -> LateEvening
    LateEvening -> Night
    Night -> LateNight
    LateNight -> Morning

-- Question #26
update msg model = case msg of
                     Tick t _ ->
                       let dt      = t - model.time
                           (px,py) = model.pos
                           (vx,vy) = model.vel
                       in  bounce { model | time = t
                                          , pos = (px + dt * vx, py + dt * vy      )
                                          , vel = (vx          , vy + dt * gravity )
                                  }

bounce model = let
                           (px,py) = model.pos
                           (vx,vy) = model.vel
               in
                 if py < -200
                   then
                     { model | pos = (px,(-200 - py) + -200)
                             , vel = (vx,-vy) }
                   else
                     model

-- Question #27
absorbEnergy (vx,vy) = let
                         scaling = 0.9
                       in
                         (scaling * vx, scaling * vy)

update msg model = case msg of
                     Tick t _ ->
                       let dt      = t - model.time
                           (px,py) = model.pos
                           (ux,uy) = model.vel
                           (vx,vy) = if px+dt*ux < -200 || px+dt*ux > 200 || py+dt*uy < -200
                                       then
                                         absorbEnergy model.vel
                                       else
                                         model.vel
                       in  bounce { model | time = t
                                          , pos = (px + dt * vx, py + dt * vy      )
                                          , vel = (vx          , vy + dt * gravity )
                                  }

bounce model = let
                           (px,py) = model.pos
                           (vx,vy) = model.vel
               in
                 if py < -200
                   then
                     { model | pos = (px,(-200 - py) + -200)
                             , vel = absorbEnergy(vx,-vy) }
                   else
                     model

-- Question #28
update msg model = case msg of
                     Tick t _ ->
                       let dt      = t - model.time
                           (px,py) = model.pos
                           (ux,uy) = model.vel
                           (vx,vy) = if px+dt*ux < -200 || px+dt*ux > 200 || py+dt*uy < -200
                                       then
                                         absorbEnergy model.vel
                                       else
                                         model.vel
                       in  bounceWall
                             <| bounce dt { model | time = t
                                                  , pos = (px + dt * vx, py + dt * vy      )
                                                  , vel = (vx          , vy + dt * gravity )
                                          }

bounce dt model = let
                    (px,py) = model.pos
                    (vx,vy) = model.vel
                    newy = (-200 - py) + -200
                    timeWithGravityPushing = sqrt ( -200 - py )
                    timeWithGravityPulling = sqrt ( -200 - newy )
                    fracSlowdown = ( timeWithGravityPushing - timeWithGravityPulling )
                                 / ( timeWithGravityPushing + timeWithGravityPulling )
                  in
                    if py < -200
                      then
                        { model | pos = (px,newy)
                                , vel = (vx,-vy - fracSlowdown * dt * gravity) }
                      else
                        model

bounceWall model = let
                           (px,py) = model.pos
                           (vx,vy) = model.vel
               in
                 if px < -200 || px > 200
                   then
                     { model | pos = (if px < 0 then -400 - px else 400 - px, py)
                             , vel = (-vx,vy) }
                   else
                     model

absorbEnergy (vx,vy) = let
                         scaling = 0.9
                       in
                         (scaling * vx, scaling * vy)

-- Question #29
fog: Float -> Float
fog x = f (g x)

-- Question #30
fogoh: Float -> Float
fogoh x = f (g ((h x)^2))

-- Question #31
double: List Float -> List Float
double list = case list of
                [] -> []
                (x::[]) -> [2*x]
                (x::xs) -> (2*x::double xs)

-- Question #32
map: (Float -> Float) -> List Float -> List Float
map f list = case list of
               (x::xs) -> (f x::map f xs)
               [] -> []

-- Question #33
sum: List Float -> Float
sum list = case list of
             (x::xs) -> x + sum xs
             [] -> 0

-- Question #34
keep50: List Float -> List Float
keep50 list = case list of
                (x::xs) -> if x > 50 then (x::keep50 xs) else (keep50 xs)
                [] -> []

-- Question #35
filter: (Float -> Bool) -> List Float -> List Float
filter f list = case list of
                  (x::xs) -> if f x == True then (x::filter f xs) else filter f xs
                  [] -> []

-- Question #36
createDict: List (String, List String) -> Dict String (List String)
createDict list = Dict.insert "e" ["eggplant", "endive", "eugenia"] (Dict.fromList fruits)

-- Question #37
listFruit: String -> Dict String (List String) -> List String
listFruit fstLetter fruitDict = fromJust (Dict.get fstLetter fruitDict)

fromJust x = case x of
  Just y -> y
  Nothing -> []

-- Question #38
insertFruit letter fruits dict = Dict.insert letter fruits dict

-- Question #41
rightAlternatingTriple : List Int -> List Int
rightAlternatingTriple numbers = case numbers of
                                   [] -> []
                                   [x] -> [x]
                                   x::y::xs -> if isIn (count numbers) (odd intList) == True
                                               then [x] ++ [3*y] ++ rightAlternatingTriple xs
                                               else [3*x] ++ [y] ++ rightAlternatingTriple xs

count list = case list of
               x::xs -> 1 + count xs
               [] -> 0

isIn number list = case list of
                    x::xs -> if x == number then True else isIn number xs
                    otherwise -> False

intList = [1..1000]

odd list = case list of
              x::y::xs -> [x] ++ odd xs
              [x] -> [x]
              [] -> []

-- Question #43
listOfShapes = [box,happy]

-- Question #44
power x y = x^y

-- Question #48
toDigitsReverse : Int -> List Int
toDigitsReverse n =
    if n <= 0 then
        []
    else
        List.append [n%10] (toDigitsReverse (Basics.floor (n/10)))

toDigits : Int -> List Int
toDigits n =
    n |> toDigitsReverse |> reverse

-- Question #50
myExpression = Add (Var "x") (Const 3)

-- Question #51
annihilate e =
  case e of
    Add u (Neg v) -> if u == v then Const 0 else Add u v
    Sqrt x -> Sqrt (annihilate x)
    Exp x -> Exp (annihilate x)
    Ln x -> Ln (annihilate x)
    Mult x y -> Mult (annihilate x) (annihilate y)
    Add x y -> Add (annihilate x) (annihilate y)
    Neg x -> Neg (annihilate x)
    otherwise             -> otherwise

-- Question #52
evalConst e =
  case e of
    Sqrt (Const c)        -> Const (sqrt c)
    Sqrt sub1             -> Sqrt (evalConst sub1)
    IntPow sub1 n         -> IntPow (evalConst sub1) n
    Exp sub1              -> Exp (evalConst sub1)
    Ln sub1               -> Ln (evalConst sub1)
    Mult sub1 sub2        -> Mult (evalConst sub1) (evalConst sub2)
    Add sub1 sub2         -> Add (evalConst sub1) (evalConst sub2)
    Neg sub1              -> Neg (evalConst sub1)
    otherwise             -> otherwise

-- Question #53
evalConst expr =
  case expr of
    Sqrt (Const a)        -> Const (a^(1/2))
    Sqrt sub1             -> Sqrt (evalConst sub1)
    IntPow (Const a) n    -> Const (a^(toFloat n))
    IntPow sub1 n         -> IntPow (evalConst sub1) n
    Exp (Const a)         -> Const (e^a)
    Exp sub1              -> Exp (evalConst sub1)
    Ln (Const a)          -> Const (logBase e a)
    Ln sub1               -> Ln (evalConst sub1)
    Mult (Const a) (Const b) -> Const (a*b)
    Mult a b              -> Mult (evalConst a) (evalConst b)
    Add (Const a) (Const b) -> Const (a+b)
    Add a b              -> Add (evalConst a) (evalConst b)
    Neg (Const a)         -> Const (-a)
    Neg a                  -> Neg (evalConst a)
    otherwise             -> otherwise

-- Question #54
subs varName constValue expr =
  case expr of
    Sqrt sub1             -> Sqrt (subs varName constValue sub1)
    IntPow sub1 n         -> IntPow (subs varName constValue sub1) n
    Exp sub1              -> Exp (subs varName constValue sub1)
    Ln sub1               -> Ln (subs varName constValue sub1)
    Mult sub1 sub2        -> Mult (subs varName constValue sub1) (subs varName constValue sub2)
    Add sub1 sub2         -> Add (subs varName constValue sub1) (subs varName constValue sub2)
    Neg sub1              -> Neg (subs varName constValue sub1)
    Var name              -> if name == varName then Const constValue else Var name
    otherwise             -> otherwise

-- Question #55
subsEval varName constValue expr = evalConst(evalConst(evalConst(evalConst (subs varName constValue expr))))

evalConst expr =
  case expr of
    Sqrt (Const a)        -> Const (a^(1/2))
    Sqrt sub1             -> Sqrt (evalConst sub1)
    IntPow (Const a) n    -> Const (a^(toFloat n))
    IntPow sub1 n         -> IntPow (evalConst sub1) n
    Exp (Const a)         -> Const (e^a)
    Exp sub1              -> Exp (evalConst sub1)
    Ln (Const a)          -> Const (logBase e a)
    Ln sub1               -> Ln (evalConst sub1)
    Mult (Const a) (Const b) -> Const (a*b)
    Mult a b              -> Mult (evalConst a) (evalConst b)
    Add (Const a) (Const b) -> Const (a+b)
    Add a b              -> Add (evalConst a) (evalConst b)
    Neg (Const a)         -> Const (-a)
    Neg a                  -> Neg (evalConst a)
    otherwise             -> otherwise

subs varName constValue expr =
  case expr of
    Sqrt sub1             -> Sqrt (subs varName constValue sub1)
    IntPow sub1 n         -> IntPow (subs varName constValue sub1) n
    Exp sub1              -> Exp (subs varName constValue sub1)
    Ln sub1               -> Ln (subs varName constValue sub1)
    Mult sub1 sub2        -> Mult (subs varName constValue sub1) (subs varName constValue sub2)
    Add sub1 sub2         -> Add (subs varName constValue sub1) (subs varName constValue sub2)
    Neg sub1              -> Neg (subs varName constValue sub1)
    Var name              -> if name == varName then Const constValue else Var name
    otherwise             -> otherwise

-- Question #56
nCopies: List Int -> List Int
nCopies lst = case lst of
                (x::xs) -> List.append (duplicateN x) (nCopies xs)
                [] -> []

-- Question #57
doubleTriple: List Int -> List Int
doubleTriple list =
    case list of
        even::odd::rest -> List.concat [[2*even], [3*odd], (doubleTriple rest)]
        even::[]        -> [2*even]
        []              -> []

-- Question #58
evenOdd: (Int -> Int) -> (Int -> Int) -> List Int -> List Int
evenOdd fEven fOdd list =
    case list of
        even::odd::rest -> List.concat [[fEven even], [fOdd odd], (evenOdd fEven fOdd rest)]
        even::[]        -> [fEven even]
        []              -> []

-- Question #59
sumElements: List Int -> List Int -> List Int
sumElements list1 list2 =
    case (list1,list2) of
        (x::rest1,y::rest2) -> List.append [x+y] (sumElements rest1 rest2)
        (x::rest,[]) -> List.append [x] (sumElements rest [])
        ([],y::rest) -> List.append [y] (sumElements rest [])
        ([],[]) -> []

--Question #60
sumFofElements: (Int -> Int) -> (Int -> Int) -> List Int -> List Int -> List Int
sumFofElements f1 f2 list1 list2 =
    case (list1,list2) of
        (x::rest1,y::rest2) -> List.append [f1 x + f2 y] (sumFofElements f1 f2 rest1 rest2)
        (x::rest,[]) -> List.append [f1 x] (sumFofElements f1 f2 rest [])
        ([],y::rest) -> List.append [f2 y] (sumFofElements f1 f2 rest [])
        ([],[]) -> []

-- Question #61
combineLists: (Int -> Int -> a) -> (Int -> Int) -> (Int -> Int) -> List Int -> List Int -> List a
combineLists fBoth f1 f2 list1 list2 =
    case (list1,list2) of
        (x::xs,y::ys) -> [fBoth (f1 x) (f2 y)] ++ combineLists fBoth f1 f2 xs ys
        otherwise -> []

-- Questions #65-69, 71-75
myShapes =
  collage 400 400
    [ circle 100
      |> filled yellow
      |> addOutline (solid 3) black
    , circle 12
      |> filled black
      |> move (-40, 10)
    , circle 12
      |> filled black
      |> move (40, 10)
    , circle 25
      |> filled black
      |> move (0, -40)
    , rect 60 35
      |> filled yellow
      |> move (0, -30)
    ]

-- Question #70
myShapes =
  collage 400 400
    [ circle 100
      |> filled myYellow
      |> addOutline (solid 3) black
    , circle 12
      |> filled black
      |> move (-40, 10)
    , circle 12
      |> filled black
      |> move (40, 10)
    , circle 25
      |> filled black
      |> move (0, -40)
    , rect 60 35
      |> filled myYellow
      |> move (0, -30)
    ]

myYellow = rgb 255 255 0

-- Question #84
solutionCurve : Stencil
solutionCurve = curve (-20,100) [Pull (-20,87.5) (0,75), Pull (20,62.5) (20,50),
                                 Pull (20,37.5) (0,25), Pull (-20,12.5) (0,0)]
                |> size 8

-- Question #95
calcPosition model = if model.time < 0
                      then (-50,0)
                      else if model.time < 10
                             then (-50 + 10 * model.time, 0)
                             else (50,0)

-- Question #96
calcPosition model = if model.time < 20
                        then (-50,0)
                      else if model.time < 45
                        then (-90 + 2 * model.time, -40 + 2 * model.time)
                      else if model.time < 70
                        then (-90 + 2 * model.time, 140 - 2 * model.time)
                      else (50,0)

-- Question #97
calcPosition model = if model.time < 3
                      then (-50,0)
                      else let angle = degrees <| 180 * (model.time)
                           in (50 * cos angle, -50 * sin angle)

-- Question #98
calcPosition model = if model.time < 2 || model.time > 6
                      then (-50,0)
                      else let angle = degrees <| 90 * (model.time)
                           in (50 * cos angle, 50 * sin angle)

-- Question #99
calcPosition model = if model.time < 1
                       then (-50,-50 * cos (Basics.pi*model.time))
                     else if model.time < 2
                       then (50 * cos (Basics.pi*model.time),50)
                     else if model.time < 3
                       then (50,50 * cos (Basics.pi*model.time))
                     else if model.time < 4
                       then (-50 * cos (Basics.pi*model.time),-50)
                     else (-50,-50)

-- Question #100
getRegisterFromComparisonCase : CPUState -> RegisterValue
getRegisterFromComparisonCase state =
    let
        (CPUState (r1,r2,r3,r4,r5,r6,r7,r8) instruction compare halt) =
            state
    in
        case compare of
            GT -> r1
            EQ -> r2
            LT -> r3
