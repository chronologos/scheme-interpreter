module Main where
import           Lib
import           System.Environment

main :: IO ()
main = do
     args <- getArgs
     test args
     return ()
