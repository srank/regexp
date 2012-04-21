module MatchRegexp (matchHere, match) where

import Regexp(Regexp(Literal, 
                      AnyChar,
                      Or, 
                      OneOrMore, 
                      ZeroOrMore,
                      Sequence,
                      Optional, 
                      AtEnd, 
                      AtStart))

import Data.List(nub)
  
match :: Regexp -> String -> [String]
match regexp text = nub $ matchRegexp regexp text

matchRegexp :: Regexp -> String -> [String]
matchRegexp _ [] = []
matchRegexp (AtStart r) text = map fst $ matchHere r text
matchRegexp r text@(x:xs)
  = map fst (matchHere r text) ++ matchRegexp r xs


matchHere :: Regexp -> String -> [(String, String)]
matchHere _ [] = []
matchHere (Literal r) text
  | take (length r) text == r  = [(r, drop (length r) text)]
  | otherwise = []
                
matchHere AnyChar (t:ts)
  = [([t], ts)]
                
matchHere (Or r1 r2) text
  = matchHere r1 text ++ matchHere r2 text
    
matchHere (OneOrMore r) text
  | null $ matched = []
  | otherwise = matched ++ getMoreMatches r matched
    where matched = matchHere r text 
          
matchHere (ZeroOrMore r) text = 
  ("", text):matchHere (OneOrMore r) text

matchHere (Sequence first second) text 
  | null firstMatches = []
  | otherwise = getSecondMatches firstMatches
    where firstMatches = matchHere first text
          getSecondMatches [] = []
          getSecondMatches ((match, remainder):ms)
            | null $ matchHere second remainder = []
            | otherwise = (knit match $ matchHere second remainder) ++ (getSecondMatches ms)

matchHere (Optional regexp) text
  | null matches = [("", text)]
  | otherwise = matches
    where matches = matchHere regexp text
          
matchHere (AtEnd regexp) text
  = filter (null . snd) $ matchHere regexp text
    

getMoreMatches :: Regexp -> [(String, String)] -> [(String, String)]
getMoreMatches _ [] = []
getMoreMatches regexp ((match, remainder):mms)
  | null $ matchHere regexp remainder = getMoreMatches regexp mms
  | otherwise = 
    (knit match (matchHere regexp remainder)) ++ 
    (getMoreMatches regexp $ (knit match $ matchHere regexp remainder) ++ mms)

knit :: String -> [(String, String)] -> [(String, String)]
knit match [] = []
knit match ((a,b):matches)
  = (match ++ a, b):knit match matches
          
