-- Expression: 3( (x+2)(x+0) + (y+2)(y+2) ) + cos ( xy + 2x + (1+0)y )

import Html exposing (text)

type Expr = Const Float
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Cos Expr

pretty : Expr -> String
pretty e = case e of
            (Const d)    -> toString d
            (Var x)      -> x
            (Add e1 e2)  -> "(" ++ (pretty e1) ++ "+" ++ (pretty e2) ++ ")"
            (Cos e1)     -> "cos(" ++ (pretty e1) ++ ")"
            (Mult e1 e2) -> "(" ++ (pretty e1) ++ "*" ++ (pretty e2) ++ ")"

trans e = case e of
            (Add a (Const 0))  -> trans a
            (Add a b)          -> Add (trans a) (trans b)
            (Mult a (Const 1)) -> trans a
            (Mult a b)         -> Mult (trans a) (trans b)
            (Cos b)            -> Cos (trans b)
            otherwise          -> e

e = Add
    ( Mult
      ( Const (3) )
      ( Add
        ( Mult
          ( Add
            ( Var ("x") )
            ( Const (2) )
          )
          ( Add
            ( Var ("x") )
            ( Const (0) )
          )
        )
        ( Mult
          ( Add
            ( Var ("y") )
            ( Const (2) )
          )
          ( Add
            ( Var ("y") )
            ( Const (2) )
          )
        )
      )
    )
    ( Cos
      ( Add
        ( Add
          ( Mult
            ( Var ("x") )
            ( Var ("y") )
          )
          ( Mult
            ( Const (2) )
            ( Var ("x") )
          )
        )
        ( Mult
          ( Add
            ( Const (1) )
            ( Const (0) )
          )
          ( Var ("y") )
        )
      )
    )

e2 = Add ( Mult ( Const (3) ) ( Add ( Mult ( Add ( Var ("x") ) ( Const (2) ) ) ( Add ( Var ("x") ) ( Const (0) ) ) ) ( Mult ( Add ( Var ("y") ) ( Const (2) ) ) ( Add ( Var ("y") ) ( Const (2) ) ) ) ) ) ( Cos ( Add ( Add ( Mult ( Var ("x") ) ( Var ("y") ) ) ( Mult ( Const (2) ) ( Var ("x") ) ) ) ( Mult ( Add ( Const (1) ) ( Const (0) ) ) ( Var ("y") ) ) ) )

main = text (toString (pretty(e)) ++ " " ++ toString (pretty(e2)))
