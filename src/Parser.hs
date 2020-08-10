{-# LANGUAGE OverloadedStrings #-}


module Parser where


import           Data.String
import           Lexer
import           Syntax
import           Text.Parsec
import qualified Text.Parsec.Expr      as Ex
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Tok

import           Data.ByteString.Short (ShortByteString)


-- Parser

binary s f assoc = Ex.Infix (reservedOp s >> return (SBinOp f)) assoc


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


int :: Parser SExpr
int = integer >>= (return . SFloat . fromInteger)


floating :: Parser SExpr
floating = float >>= (return . SFloat)


expr :: Parser SExpr
expr = Ex.buildExpressionParser table factor


variable :: Parser SExpr
variable = identifier >>= (return . SVar)


function :: Parser SExpr
function = reserved "def" >> SFunction <$> identifier <*> (parens $ many variable) <*> expr


extern :: Parser SExpr
extern = reserved "extern" >> SExtern <$> identifier <*> (parens $ many variable)


call :: Parser SExpr
call = SCall <$> identifier <*> (parens $ commaSep expr)


factor :: Parser SExpr
factor = try floating
    <|> try int
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr


defn :: Parser SExpr
defn = try extern
    <|> try function
    <|> expr


contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r


topLevel :: Parser [SExpr]
topLevel = many $ do
    def <- defn
    reservedOp ";"
    return def


parseExpr :: String -> Either ParseError Expr
parseExpr string =
    case parse (contents expr) "<stdin>" string of
        Left err   -> Left err
        Right sexp -> Right (convert sexp)


parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel string =
    case parse (contents topLevel) "<stdin>" string of
        Left err    -> Left err
        Right sexps -> Right (map convert sexps)



-- Convert AST to ByteString


convert :: SExpr -> Expr
convert (SFloat doub)        = Float doub
convert (SBinOp op sxp sxp') = BinOp op (convert sxp) (convert sxp')
convert (SVar str)           = Var (fromString str)
convert (SCall str sxps)    = Call (fromString str) (map convert sxps)
convert (SFunction str sxps sxp) = Function (fromString str) (map convert sxps) (convert sxp)
convert (SExtern str sxps) = Extern (fromString str) (map convert sxps)
