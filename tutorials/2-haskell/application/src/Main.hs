module Main where


import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           Lib          (ultimateAnswer)
import           Text.Printf  (printf)


main :: IO ()
main = do
    T.IO.putStrLn . T.pack $ printf template ultimateAnswer
    where
    template =
        "Answer to the Ultimate Question of Life,\n\
        \    the Universe, and Everything: %d"
