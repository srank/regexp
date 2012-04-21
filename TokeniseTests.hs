module TokeniseTests(tokeniserTests) where
import Tokenise


tokeniserTests = runTokeniseTests tokeniseTests

tokeniseTests :: [(String, [Token])]
tokeniseTests = [("xx", [Text "xx"]),
                 ("(", [OpenBracket]),
                 (")", [CloseBracket]),
                 ("^abc|de+f*$", [Start, Text "abc", Either, Text "de",
                                  Plus, Text "f", Star, End]),
                 ("\\$12\\.34", [Text "$12.34"])
                ]

runTokeniseTests :: [(String, [Token])] -> [(String, [Token], [Token])]
runTokeniseTests [] = []
runTokeniseTests ((text, expected):xs)
  | actual == expected = runTokeniseTests xs
  | otherwise = (text, actual, expected):runTokeniseTests xs
    where actual = tokenise text