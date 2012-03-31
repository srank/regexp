module Main (main) where

import System( getArgs )
import Grep (getMatch)

main :: IO ()
main = do
  args <- getArgs
  if length args < 3 then 
    do
      print "oops"
        else do print args
                print $ getMatch (args!!1) (args!!2)
  