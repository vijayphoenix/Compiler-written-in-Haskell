module Language ( 
    haskullstyle, 
    haskulldef
) where 

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language


-- | This is a minimal token definition for Haskull style languages. It
-- defines the style of comments, valid identifiers and case
-- sensitivity. It does not define any reserved words or operators.

haskullstyle :: LanguageDef st
haskullstyle = emptyDef { 
      commentStart      = "{-"
    , commentEnd        = "-}"
    , commentLine       = "//"
    , nestedComments    = False
    , reservedNames     = []
    , reservedOpNames   = []
    , caseSensitive     = True
    , identStart        = letter <|> char '_'
    , identLetter       = alphaNum <|> oneOf "_'"
}


-- | The language definition for the Haskull language.
haskulldef = haskullstyle { 
    reservedOpNames   = ["+", "/", "-", "*", ";", "="], 
    reservedNames     = ["int", "char", "def", "extern", "string"]
}
    