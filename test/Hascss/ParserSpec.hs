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
        describe "Lengths" $ do 
            let parse = parseMaybe lengthP
            let shouldParseTo a b = parse a `shouldBe` pure b
            let shouldNotParse a = parse a `shouldBe` Nothing
            it "parses easy lengths" $ 
                "10rem" `shouldParseTo` Length 10 "rem"
            it "parses lengths with identifiers and decimals" $
                "10.0px" `shouldParseTo` Length 10 "px"
            it "parses negative lengths with identifiers" $
                "-10em" `shouldParseTo` Length (-10) "em"
            it "parses zero without a unit" $
                "0" `shouldParseTo` Length 0 ""
            it "doesn't parse without units otherwise" $
                shouldNotParse "10"
            