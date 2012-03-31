module Main (main) where

import System( getArgs )

main :: IO ()
main = do
  args <- getArgs
  print $ show args
  