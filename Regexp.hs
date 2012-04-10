module Regexp (Regexp(Literal, Or), matchHere) where

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