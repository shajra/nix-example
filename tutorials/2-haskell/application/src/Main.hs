{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}


module Main where

import           Protolude

import           Text.Printf (printf)

import           Lib         (ultimateAnswer)


main :: IO ()
main = putStrLn @ [Char] . printf template $ ultimateAnswer
    where
    template =
        "Answer to the Ultimate Question of Life,\n\
        \    the Universe, and Everything: %d"
