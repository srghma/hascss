module Hascss.ParserSpec where
    import Test.Hspec
    import Text.Megaparsec (parseMaybe)
    import Hascss.Parser
    import Hascss.AST

    spec = do
        describe "Identifier" $ do
            let shouldParse a = (parseMaybe identifier a) `shouldBe` pure a
            let shouldNotParse a = (parseMaybe identifier a) `shouldBe` Nothing
            it "allows valid, normal identifiers" $
                shouldParse "foo"
            it "disallows starting numbers" $
                shouldNotParse "1test"
            it "allows middle numbers/symbols" $ do
                shouldParse "t1t"
                shouldParse "border-width"
                shouldParse "i18n__box"
            it "allows starting underscores but not doubled" $ do
                shouldParse "_t"
                shouldNotParse "_"
                shouldNotParse "__"
            it "allows starting dashes but not doubled" $ do
                shouldParse "-t"
                shouldNotParse "-"
                shouldNotParse "__"
        describe "Selectors" $ do 
            let parse = parseMaybe selector
            let shouldParseTo a b = parse a `shouldBe` pure b
            it "parses class selectors" $
                ".foo" `shouldParseTo` Selector Class "foo"
            it "parses id selectors" $ 
                "#foo" `shouldParseTo` Selector Id "foo"
            it "parses element selectors" $ 
                "foo" `shouldParseTo` Selector Element "foo"
        