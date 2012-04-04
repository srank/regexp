-- Data structure for regular expressions

data Regexp = Literal String | 
              Or Regexp Regexp |
              OneOrMore Regexp |
              ZeroOrMore Regexp |
              AtStart Regexp |
              AtEnd Regexp |
              OneOf [Regexp] |
              Optional Regexp
              
matchRegexp :: Regexp -> String -> Maybe String
matchRegexp (Literal r) text@(c:cs)
  | length r > length text = Nothing
  | take (length r) text == r  = Just r
  | otherwise = matchRegexp (Literal r) cs
