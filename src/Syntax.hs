module Syntax where


-- Type Definitions

type Name = String


data Expr = Float Double
    | BinOp Op Expr Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)


data Op = Plus
    | Minus
    | Times
    | Divide
    | Equal
    | Less
    deriving (Eq, Ord, Show)
