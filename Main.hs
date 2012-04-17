module Main (main) where

import System(getArgs)

import ParseRegexp(matchIt)

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then 
      print $ "oops; only " ++ (show $ length args) ++ " args"
        else print $ matchIt (args!!0) (args!!1)

  