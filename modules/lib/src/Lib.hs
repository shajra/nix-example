module Lib where


import           System.IO (getChar)


waitForKey :: IO ()
waitForKey = const () <$> getChar
