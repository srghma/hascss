module Main where

import Hascss.Parser

import Text.Megaparsec (parseTest)

main :: IO ()
main = do 
    parseTest ruleBodyItem "rgba(1,1,1,0.5)"
