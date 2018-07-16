module Main where

import Hascss.Parser

import Text.Megaparsec (parseTest)

main :: IO ()
main = do 
    parseTest rule "font-size: 10em;"
