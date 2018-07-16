module Hascss.Parser where

    import Hascss.AST
    import Text.Megaparsec
    import qualified Text.Megaparsec.Char.Lexer as L
    import Text.Megaparsec.Char
    import Data.Void

    type Parser = Parsec Void String

    sc :: Parser ()
    sc = L.space space1 lineCmnt blockCmnt  
        where
            lineCmnt  = L.skipLineComment "//"
            blockCmnt = L.skipBlockComment "/*" "*/"

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    identifier :: Parser Stringish
    identifier = (++) <$> starting <*> many identChar
        where
            starting :: Parser Stringish
            starting = normalStart <|> underscoreStart
            normalStart = (:) <$> letterChar <*> pure []
            underscoreStart :: Parser Stringish
            underscoreStart = do
                u <- otherSyms
                i <- letterChar
                pure [u, i]
            identChar :: Parser Char
            identChar = alphaNumChar <|> otherSyms
            otherSyms = char '-' <|> char '_'
    
    selector :: Parser Selector
    selector = try classSelector <|> try idSelector <|> try elementSelector
        where
            classSelector = char '.' >> Selector Class <$> identifier
            idSelector = char '#' >> Selector Id <$> identifier
            elementSelector = Selector Element <$> identifier
    
    doubleP :: Parser Double 
    doubleP = try pureDouble <|> integralDouble
        where
            pureDouble = L.signed (pure ()) L.float
            integralDouble = fromIntegral <$> L.signed (pure ()) L.decimal 

    lengthP :: Parser Length
    lengthP = do
        f <- doubleP
        if f == 0.0 then
            pure $ Length 0 ""
        else Length f <$> identifier

    lengthBody :: Parser RuleBodyItem
    lengthBody = LengthBody <$> lengthP

    ruleBodyItem :: Parser RuleBodyItem
    ruleBodyItem = lexeme lengthBody 

    ruleBody :: Parser RuleBody 
    ruleBody = ruleBodyItem `manyTill` char ';'

    rule :: Parser Rule
    rule = do 
        n <- lexeme identifier
        char ':'
        Rule n <$> ruleBody

    