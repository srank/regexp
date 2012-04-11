module Regexp (Regexp(Literal, 
                      Or, 
                      OneOrMore, 
                      ZeroOrMore,
                      Sequence,
                      Optional), 
               matchHere) where

-- Data structure for regular expressions
data Regexp = Literal String | 
              Or Regexp Regexp |
              OneOrMore Regexp |
              ZeroOrMore Regexp |
              Sequence Regexp Regexp |
              Optional Regexp |            
              AtStart Regexp |
              AtEnd Regexp
              deriving Show


matchHere :: Regexp -> String -> [(String, String)]
matchHere (Literal r) text
  | take (length r) text == r  = [(r, drop (length r) text)]
  | otherwise = []
                
matchHere (Or r1 r2) text
  = matchHere r1 text ++ matchHere r2 text
    
matchHere (OneOrMore r) text
  | null $ matched = []
  | otherwise = matched ++ getMoreMatches r matched
    where matched = matchHere r text 
          
matchHere (ZeroOrMore r) text
  | null $ matched = [("", text)]
  | otherwise = matched ++ getMoreMatches r matched
    where matched = matchHere r text
          
matchHere (Sequence r1 r2) text 
  | null matchedR1 = []
  | otherwise = getSecondMatches r2 matchedR1
    where matchedR1 = matchHere r1 text
          getSecondMatches regexp [] = []
          getSecondMatches regexp ((match, remainder):ms)
            | null $ matchHere regexp remainder = []
            | otherwise = (knit match $ matchHere regexp remainder) ++ (getSecondMatches regexp ms)

matchHere (Optional regexp) text
  | null matches = [("", text)]
  | otherwise = matches
    where matches = matchHere regexp text

getMoreMatches :: Regexp -> [(String, String)] -> [(String, String)]
getMoreMatches _ [] = []
getMoreMatches regexp ((match, remainder):mms)
  | null $ matchHere regexp remainder = []
  | otherwise = knit match (matchHere regexp remainder)

knit :: String -> [(String, String)] -> [(String, String)]
knit match [] = []
knit match ((a,b):matches)
  = (match ++ a, b):knit match matches
    
          
