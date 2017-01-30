{-# LANGUAGE OverloadedStrings #-}


module Main where


import           System.IO                (getChar)
import           System.Remote.Monitoring (forkServer)

import           Lib                      (waitForKey)


main :: IO ()
main = do
    forkServer "0.0.0.0" 8081
    putStrLn "EKG running on http://localhost:8081"
    putStrLn "hit any key to quit"
    waitForKey
