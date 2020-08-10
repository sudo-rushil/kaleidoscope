module Syntax where


import           Data.ByteString.Short


-- Type Definitions

type Name = ShortByteString


data Expr = Float Double
    | BinOp Op Expr Expr
    | Var ShortByteString
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)


data SExpr = SFloat Double
    | SBinOp Op SExpr SExpr
    | SVar String
    | SCall String [SExpr]
    | SFunction String [SExpr] SExpr
    | SExtern String [SExpr]
    deriving (Eq, Ord, Show)


data Op = Plus
    | Minus
    | Times
    | Divide
    | Equal
    | Less
    deriving (Eq, Ord, Show)
