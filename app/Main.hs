module Main where

import Hascss.Parser

import Text.Megaparsec (parseTest)

main :: IO ()
main = do 
    parseTest ast ".foo {\nborder: 10px;\n}"
