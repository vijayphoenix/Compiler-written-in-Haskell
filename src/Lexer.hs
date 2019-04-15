module Lexer (
    lexer, 
    ident,
    reserved,
    reservedOp,
    charLiteral,
    Parser(..),
    parse
) where 

import Language
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.String (Parser)

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser haskulldef

ident :: Parser String
ident = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer 

reservedOp :: String -> Parser () 
reservedOp = Tok.reservedOp lexer 

charLiteral :: Parser Char 
charLiteral = Tok.charLiteral lexer 