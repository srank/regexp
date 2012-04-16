module Tests (main) where
import qualified Grep (match)
import Regexp
import ParseRegexp

tokeniseTests :: [(String, [Token])]
tokeniseTests = [("xx", [Text "xx"]),
                 ("(", [OpenBracket]),
                 (")", [CloseBracket]),
                 ("^abc|de+f*$", [Start, Text "abc", Either, Text "de",
                                  Plus, Text "f", Star, End]),
                 ("\\$12:34", [Text "$12:34"])
                ]

runTokeniseTests :: [(String, [Token])] -> [(String, [Token], [Token])]
runTokeniseTests [] = []
runTokeniseTests ((text, expected):xs)
  | actual == expected = runTokeniseTests xs
  | otherwise = (text, actual, expected):runTokeniseTests xs
    where actual = tokenise text
          
parseTests :: [([Token], Maybe Regexp)]
parseTests =
  [([Text "abc"], Just $ Literal "abc"),
   ([Text "xyz", Dot], Just $ Sequence (Literal "xyz") AnyChar),
   ([Start, Text "a"], Just $ AtStart (Literal "a")),
   ([OpenBracket, Text "abc", CloseBracket], Just $ Literal "abc"),
   ([OpenBracket, Text "abc", CloseBracket, Text "xyz"], 
    Just $ Sequence (Literal "abc") (Literal "xyz")),
   ([OpenBracket, Text "ww", OpenBracket, Text "qq", CloseBracket, CloseBracket],
    Just $ Sequence (Literal "ww") (Literal "qq"))
  ]
  
runParseTests [] = []
runParseTests ((tokens, expected):ps)
  | actual == expected = runParseTests ps
  | otherwise = (tokens, actual, expected):runParseTests ps
    where actual = parse tokens

matchHereTests :: [(Regexp, String, [(String, String)])]
matchHereTests = [(Literal "abc", "", []),
                  (Literal "x", "xbc", [("x", "bc")]),
                  (Or (Literal "x") (Literal ("y")), "abc", []),
                  (Or (Literal "x") (Literal ("y")), "xbc", 
                    [("x", "bc")]),
                  (OneOrMore (Literal "x"), "xxbc", 
                    [("x", "xbc"), ("xx", "bc")]),
                  (OneOrMore (Literal "x"), "yxx", []),
                  (ZeroOrMore (Literal "x"), "xxbc", 
                    [("x", "xbc"), ("xx", "bc")]),
                  (ZeroOrMore (Literal "x"), "yxx", [("", "yxx")]),
                  (Sequence (Literal "x") (Literal "z"), "xyz", []),
                  (Sequence (Literal "y") (Literal "z"), "xyz", []),
                  (Sequence (Literal "x") (Literal "y"), "xyz", [("xy", "z")]),
                  (Optional (Literal "x"), "xyz", [("x", "yz")]),
                  (Optional (Literal "x"), "yz", [("", "yz")]),
                  (AtEnd (Literal "x"), "xy", []),
                  (AtEnd (Literal "x"), "x", [("x", "")]),
                  (AnyChar, "xyz", [("x", "yz")])
                 ]
                 
matchTests = [(AtStart (Literal "x"), "xyz", ["x"])]

runNewTests = runTests matchHere matchHereTests
              

runTests :: (Show a, Eq a) => (Regexp -> String -> a) -> 
        [(Regexp, String, a)] -> 
        [(Regexp, String, a, a)]
runTests f [] = []
runTests f ((r, t, expected):ss)
  | actual == expected = runTests f ss
  | otherwise = (r, t, actual, expected): runTests f ss
    where actual = f r t

main :: IO ()
main = do
        print runNewTests 
        print $ runTests match matchTests
        print $ runTokeniseTests tokeniseTests
        print $ runParseTests parseTests
        
        
oldTests = [("xy", "123xy456", Just "xy"),
         ("z", "xyz", Just "z"),
         ("x.y", "xzy", Just "xzy"),
         ("^1234", "123456", Just "1234"),
         ("12^abc", "abc", Nothing),
         ("abc^", "abc", Nothing),
         ("a+", "aaa", Just "aaa"),
         ("c+", "abccd", Just "cc"),
         ("c+d+", "abccd", Just "ccd"),
         ("c+", "x", Nothing),
         ("c+x", "x", Nothing),
         ("c?x", "x", Just "x"),
         ("c?x", "cx", Just "cx"),
         ("a.+b", "accb", Just "accb"),
         ("a.+", "axxx", Just "ax"),
         ("a*b", "aab", Just "aab"),
         ("a*b", "b", Just "b"),
         ("xx$", "xx", Just "xx"),
         ("x$", "xyz", Nothing),
         ("$", "abc", Just ""),
         ("^", "xyz", Just ""),
         (".$", "x", Just "x"),
         (".$", "", Nothing)      
        ]
                

                                            
