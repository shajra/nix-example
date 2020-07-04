module Main where


import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           Text.Printf  (printf)

import           Lib          (ultimateAnswer)


main :: IO ()
main = T.IO.putStrLn . T.pack $ printf template ultimateAnswer
    where
    template =
        "Answer to the Ultimate Question of Life,\n\
        \    the Universe, and Everything: %d"
