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
matchRegexp r text@(_:xs)
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
  | null matched = []
  | otherwise = matched ++ getMoreMatches r matched
    where matched = matchHere r text 
          
matchHere (ZeroOrMore r) text = 
  ("", text):matchHere (OneOrMore r) text

matchHere (Sequence first second) text 
  | null firstMatches = []
  | otherwise = getSecondMatches firstMatches
    where firstMatches = matchHere first text
          getSecondMatches [] = []
          getSecondMatches ((matched, remainder):ms)
            | null $ matchHere second remainder = []
            | otherwise = knit matched (matchHere second remainder) ++ 
                          getSecondMatches ms

matchHere (Optional regexp) text
  | null matches = [("", text)]
  | otherwise = matches
    where matches = matchHere regexp text
          
matchHere (AtEnd regexp) text =
  filter (null . snd) $ matchHere regexp text
    
matchHere (AtStart regexp) _ =
  error "Misuse of ^"

getMoreMatches :: Regexp -> [(String, String)] -> [(String, String)]
getMoreMatches _ [] = []
getMoreMatches regexp ((matched, remainder):mms)
  | null $ matchHere regexp remainder = getMoreMatches regexp mms
  | otherwise = 
    knit matched (matchHere regexp remainder) ++ 
    getMoreMatches regexp (knit matched (matchHere regexp remainder) ++ mms)

knit :: String -> [(String, String)] -> [(String, String)]
knit _ [] = []
knit matched ((a,b):matches)
  = (matched ++ a, b):knit matched matches
          
