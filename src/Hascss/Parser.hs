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
    lengthP = try $ do
        f <- doubleP
        if f == 0.0 then
            pure $ Length 0 ""
        else Length f <$> identifier

    lengthBody :: Parser RuleBodyItem
    lengthBody = LengthBody <$> lengthP

    literalBody :: Parser RuleBodyItem
    literalBody = LiteralBody <$> identifier

    percentageBody :: Parser RuleBodyItem
    percentageBody = try $ do
        f <- doubleP
        char '%'
        pure $ PercentageBody f

    numberBody :: Parser RuleBodyItem
    numberBody = NumberBody <$> doubleP

    funcallBody :: Parser RuleBodyItem
    funcallBody = try $ do
        name <- identifier
        char '('
        args <- ruleBodyItem `sepBy` lexeme (char ',')
        char ')'
        pure $ FuncallBody name args

    ruleBodyItem :: Parser RuleBodyItem
    ruleBodyItem = lexeme ( funcallBody
                        <|> lengthBody
                        <|> percentageBody 
                        <|> numberBody
                        <|> literalBody)

    ruleBody :: Parser RuleBody 
    ruleBody = ruleBodyItem `manyTill` char ';'

    rule :: Parser Rule
    rule = do 
        n <- identifier
        lexeme $ char ':'
        Rule n <$> ruleBody

    