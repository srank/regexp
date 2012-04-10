module Regexp (Regexp(Literal), matchRegexp, matchHere) where

-- Data structure for regular expressions
data Regexp = Literal String | 
              Or Regexp Regexp |
              OneOrMore Regexp |
              ZeroOrMore Regexp |
              AtStart Regexp |
              AtEnd Regexp |
              OneOf [Regexp] |
              Optional Regexp
              deriving Show

-- query: do we need to think about matching "here" vs matching
-- anywhere?
-- "(x|y)z" matches "xz", "yz", "aaaxzbbb"
-- "abc" matches "xyzabcdef"
-- once we start matching, it has to be matchHere from then on, 
-- I think

matchRegexp :: Regexp -> String -> Maybe String
matchRegexp (Literal r) text@(c:cs)
  | length r > length text = Nothing
  | take (length r) text == r  = Just r
  | otherwise = matchRegexp (Literal r) cs

matchRegexp (Or a b) text
  | matchRegexp a text /= Nothing = matchRegexp a text
  | matchRegexp b text /= Nothing = matchRegexp b text
  | otherwise = Nothing

matchRegexp (OneOrMore x) text = Nothing

matchRegexp (ZeroOrMore x) text = Nothing
matchRegexp (AtStart x) text = Nothing
matchRegexp (AtEnd x) text = Nothing
matchRegexp (OneOf (r:rs)) text = Nothing
matchRegexp (Optional r) text = Nothing

matchHere :: Regexp -> String -> [(String, String)]
matchHere (Literal r) text
  | take (length r) text == r  = [(r, drop (length r) text)]
  | otherwise = []