module Main where

import Hascss.Parser

import Text.Megaparsec (parseTest')

main :: IO ()
main = do 
    parseTest' ast ".foo {\n .bar { \n .baz { \n margin-white: 10; } } }"
