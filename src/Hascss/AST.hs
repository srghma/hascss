{-# LANGUAGE DeriveGeneric #-}

module Hascss.AST where

    import GHC.Generics (Generic)
    
    type Stringish = String

    type Number = Double

    data SelectorType
        = Class
        | Id
        | Element 
        deriving (Eq, Show, Ord, Generic)
    
    data Selector = Selector SelectorType Stringish
        deriving (Eq, Show, Ord, Generic)

    data Length = Length { measure :: Number, unit :: String }
        deriving (Eq, Show, Ord, Generic)

    data RuleBodyItem
        = LengthBody Length
        | PercentageBody Number
        | LiteralBody Stringish
        | NumberBody Number
        | FuncallBody Stringish [RuleBodyItem]
        | VarBody Stringish
        deriving (Eq, Show, Ord, Generic)

    type RuleBody = [RuleBodyItem]

    data Rule = Rule Stringish RuleBody
        deriving (Eq, Show, Ord, Generic)

    data AST
        = BlockDefn Selector [AST]
        | RuleBlock Rule
        | NestedBlock AST
        | VariableDefn String RuleBody
        deriving (Eq, Show, Ord, Generic)

    ruleName (Rule n b) = n
    