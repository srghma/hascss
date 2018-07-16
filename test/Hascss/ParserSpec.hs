module Hascss.ParserSpec where
    import Test.Hspec
    import Text.Megaparsec (parseMaybe)
    import Hascss.Parser
    import Hascss.AST

    spec = do
        let shouldParseTo' parser a b = parser a `shouldBe` pure b
        let shouldNotParse' parser a = parser a `shouldBe` Nothing 
        describe "Identifier" $ do
            let shouldParse a = parseMaybe identifier a `shouldBe` pure a
            let shouldNotParse a = parseMaybe identifier a `shouldBe` Nothing
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
            let shouldParseTo = shouldParseTo' parse
            it "parses class selectors" $
                ".foo" `shouldParseTo` Selector Class "foo"
            it "parses id selectors" $ 
                "#foo" `shouldParseTo` Selector Id "foo"
            it "parses element selectors" $ 
                "foo" `shouldParseTo` Selector Element "foo"
        describe "Lengths" $ do 
            let parse = parseMaybe lengthP
            let shouldParseTo = shouldParseTo' parse 
            let shouldNotParse = shouldNotParse' parse
            it "parses easy lengths" $ 
                "10rem" `shouldParseTo` Length 10 "rem"
            it "parses lengths with identifiers and decimals" $
                "10.0px" `shouldParseTo` Length 10 "px"
            it "parses negative lengths with identifiers" $
                "-10em" `shouldParseTo` Length (-10) "em"
            it "parses zero without a unit" $
                "0" `shouldParseTo` Length 0 ""
            it "requires units for nonzero lengths" $
                shouldNotParse "10"
        describe "Rule body items" $ do
            let parse = parseMaybe ruleBodyItem
            let shouldParseTo = shouldParseTo' parse
            let shouldNotParse = shouldNotParse' parse
            it "parses length items" $
                "10px" `shouldParseTo` LengthBody (Length 10 "px")
            it "parses literal items" $
                "red" `shouldParseTo` LiteralBody "red"
            it "parses percentage items" $
                "10%" `shouldParseTo` PercentageBody 10
            it "parses funcall items" $
                "rgba(1,1,1,0.5)" `shouldParseTo`
                    FuncallBody "rgba" (map NumberBody [1, 1, 1, 0.5])
        describe "Rule bodies" $ do
            let parse = parseMaybe ruleBody
            let shouldParseTo = shouldParseTo' parse
            let shouldNotParse = shouldNotParse' parse 
            it "parses multi length bodies" $
                "10px 20px;" `shouldParseTo` [LengthBody $ Length 10 "px", LengthBody $ Length 20 "px"]
            it "parses single length bodies" $ 
                "10px;" `shouldParseTo` [LengthBody $ Length 10 "px"]
        describe "Rules" $ do
            let parse = parseMaybe rule
            let shouldParseTo = shouldParseTo' parse
            it "parses simple body-rule pairs" $ do
                "font-size: 10em;" `shouldParseTo` Rule "font-size" [(LengthBody $ Length 10 "em")]