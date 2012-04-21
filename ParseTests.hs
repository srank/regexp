module ParseTests(parseTestResults) where

import Regexp
import ParseRegexp(parse)
import Tokenise


parseTestResults :: [([Token], Maybe Regexp, Maybe Regexp)]
parseTestResults = runParseTests parseTests

parseTests :: [([Token], Maybe Regexp)]
parseTests =
  [([Text "abc"], Just $ Literal "abc"),
   ([Text "xyz", Dot], Just $ Sequence (Literal "xyz") AnyChar),
   ([Start, Text "a"], Just $ AtStart (Literal "a")),
   ([OpenBracket, Text "abc", CloseBracket], Just $ Literal "abc"),
   ([OpenBracket, Text "abc", CloseBracket, Text "xyz"], 
    Just $ Sequence (Literal "abc") (Literal "xyz")),
   ([OpenBracket, Text "ww", OpenBracket, Text "qq", CloseBracket, 
     CloseBracket],
    Just $ Sequence (Literal "ww") (Literal "qq")),
   ([Text "ww", End], Just $ AtEnd (Literal "ww")),
   ([OpenBracket, Text "z", OpenBracket, Text "a", Text "x", 
     CloseBracket, CloseBracket, Text "b"], 
    Just $ Sequence (Sequence (Literal "z") 
                     (Sequence (Literal "a") (Literal "x"))) 
    (Literal "b")),
   ([OpenBracket, Text "a", CloseBracket, Text "b"], 
    Just $ Sequence (Literal "a") (Literal "b")),
   ([OpenBracket, Text "a", Text "b", CloseBracket, Plus], 
    Just $ OneOrMore (Sequence (Literal "a") (Literal "b"))),
   ([Text "a", QuestionMark], Just $ Optional $ Literal "a")
  ]
  
runParseTests :: [([Token], Maybe Regexp)] -> 
                 [([Token], Maybe Regexp, Maybe Regexp)]
runParseTests [] = []
runParseTests ((tokens, expected):ps)
  | actual == expected = runParseTests ps
  | otherwise = (tokens, actual, expected):runParseTests ps
    where actual = parse tokens
