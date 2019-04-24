module Lexer (
    lexer, 
    ident,
    reserved,
    reservedOp,
    stringLiteral,
    integer,
    paren,
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

-- | This is the string tokenizer
--  Supports parsing of an string.
stringLiteral :: Parser String 
stringLiteral = Tok.stringLiteral lexer 

-- | This is the integer tokenizer
--  Supports parsing of an integer.
--  Hexadecimal form :: Number should be prefixed by "0x" or "0X".
--  Octal form       :: Number should be prefixed by "0o" or "0O".
--  Returns the value of the number.
integer :: Parser Integer 
integer = Tok.integer lexer

paren :: Parser a -> Parser a 
paren = Tok.parens lexer
