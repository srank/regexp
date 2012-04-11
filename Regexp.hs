module Regexp (Regexp(Literal, Or, OneOrMore), matchHere) where

-- Data structure for regular expressions
data Regexp = Literal String | 
              Or Regexp Regexp |
              OneOrMore Regexp |
              ZeroOrMore Regexp |
              AtStart Regexp |
              AtEnd Regexp |
              Sequence [Regexp] |
              Optional Regexp
              deriving Show


matchHere :: Regexp -> String -> [(String, String)]
matchHere (Literal r) text
  | take (length r) text == r  = [(r, drop (length r) text)]
  | otherwise = []
                
matchHere (Or r1 r2) text
  = matchHere r1 text ++ matchHere r2 text
    
matchHere (OneOrMore r) text
  | null $ matchHere r text = []
  | otherwise = ms ++ getMoreMatches r ms
    where ms = matchHere r text 
          getMoreMatches _ [] = []
          getMoreMatches regexp ((match, remainder):mms)
            | null $ matchHere regexp remainder = []
            | otherwise = knit match (matchHere regexp remainder)
          knit match [] = []
          knit match ((a,b):matches)
            = (a ++ match, b):knit match matches