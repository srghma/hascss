{-# LANGUAGE DeriveGeneric #-}

module Hascss.AST where

    import GHC.Generics (Generic)
    
    type Stringish = String

    data SelectorType
        = Class
        | Id
        | Element 
        deriving (Eq, Show, Ord, Generic)
    
    data Selector = Selector SelectorType Stringish
        deriving (Eq, Show, Ord, Generic)

    data Length = Length { measure :: Double, unit :: String }
        deriving (Eq, Show, Ord, Generic)

    data RuleBodyItem = LengthBody Length
        deriving (Eq, Show, Ord, Generic)

    type RuleBody = [RuleBodyItem]

    data Rule = Rule Stringish RuleBody
        deriving (Eq, Show, Ord, Generic)

    data AST
        = Block Selector [AST]
        | RuleBlock Rule
        deriving (Eq, Show, Ord, Generic)

    ruleName (Rule n b) = n
    