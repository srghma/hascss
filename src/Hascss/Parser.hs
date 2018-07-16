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
    identifier = (++) <$> starting <*> many identChar <?> "Identifier"
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
    
    varName :: Parser Stringish
    varName = try (char '$' >> identifier)

    labelTry s p = label s $ try p 

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
    funcallBody = labelTry "Function call" $ do
        name <- identifier
        char '('
        args <- ruleBodyItem `sepBy` lexeme (char ',')
        char ')'
        pure $ FuncallBody name args

    varBody :: Parser RuleBodyItem
    varBody = VarBody <$> varName <?> "Variable Name"

    ruleBodyItem :: Parser RuleBodyItem
    ruleBodyItem = lexeme ( funcallBody
                        <|> lengthBody
                        <|> percentageBody 
                        <|> numberBody
                        <|> varBody
                        <|> literalBody)

    ruleBody :: Parser RuleBody 
    ruleBody = ruleBodyItem `someTill` char ';'

    rule :: Parser Rule
    rule = do 
        n <- identifier
        lexeme $ char ':'
        Rule n <$> ruleBody
    
    astBlock :: Parser AST
    astBlock = do 
        sel <- lexeme selector
        lexeme $ char '{'
        body <- many $ lexeme ast
        lexeme $ char '}'
        pure $ BlockDefn sel body

    astRule :: Parser AST
    astRule = RuleBlock <$> rule

    astNested :: Parser AST
    astNested = NestedBlock <$> try (char '&' >> astBlock)

    astVarDef :: Parser AST
    astVarDef = try $ do 
        n <- varName
        lexeme $ char ':'
        VariableDefn n <$> ruleBody

    ast :: Parser AST
    ast = astRule <|> try astBlock <|> astNested <|> astVarDef