{-# LANGUAGE OverloadedStrings #-}


module Parser where


import           Lexer
import           Syntax
import           Text.Parsec
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as Tok


-- Parser

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc


table =
    [
        [ binary "*" Times Ex.AssocLeft
        , binary "/" Divide Ex.AssocLeft
        ]
    ,
        [ binary "+" Plus Ex.AssocLeft
        , binary "-" Minus Ex.AssocLeft
        ]
    ,
        [ binary "=" Equal Ex.AssocLeft
        , binary "<" Less Ex.AssocLeft
        ]
    ]


-- int :: Parser Expr
-- int = integer >>= (return . Float . fromInteger)
--
--
-- floating :: Parser Expr
-- floating = float >>= (return . Float)
--
--
-- expr :: Parser Expr
-- expr = Ex.buildExpressionParser table factor
--
--
-- variable :: Parser Expr
-- variable = identifier >>= (return . Var)
--
--
-- function :: Parser Expr
-- function = reserved "def" >> Function <$> identifier <*> (parens $ many variable) <*> expr
--
--
-- extern :: Parser Expr
-- extern = reserved "extern" >> Extern <$> identifier <*> (parens $ many variable)
--
--
-- call :: Parser Expr
-- call = Call <$> identifier <*> (parens $ commaSep expr)
--
--
-- factor :: Parser Expr
-- factor = try floating
--     <|> try int
--     <|> try extern
--     <|> try function
--     <|> try call
--     <|> variable
--     <|> parens expr
--
--
-- defn :: Parser Expr
-- defn = try extern
--     <|> try function
--     <|> expr
--
--
-- contents :: Parser a -> Parser a
-- contents p = do
--     Tok.whiteSpace lexer
--     r <- p
--     eof
--     return r
--
--
-- topLevel :: Parser [Expr]
-- topLevel = many $ do
--     def <- defn
--     reservedOp ";"
--     return def
--
--
-- parseExpr :: String -> Either ParseError Expr
-- parseExpr = parse (contents expr) "<stdin>"
--
--
-- parseTopLevel :: String -> Either ParseError [Expr]
-- parseTopLevel = parse (contents topLevel) "<stdin>"
