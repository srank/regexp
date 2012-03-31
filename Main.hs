module Main (main) where

import System( getArgs )
import Grep (match)

main :: IO ()
main = do
  args <- getArgs
  if length args < 3 then 
      print "oops"
        else print $ match (args!!1) (args!!2)
  