module Main (main) where

import System(getArgs)

import ParseRegexp(tokenise, parse)
import MatchRegexp(match)

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then 
      print $ "oops; only " ++ (show $ length args) ++ " args"
        else print $ matchIt (args!!0) (args!!1)

matchIt =
  match . getRegexp . parse . tokenise
  where getRegexp (Just r) = r 
        getRegexp Nothing = error "Parse error"
  