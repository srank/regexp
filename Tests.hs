module Tests (main) where
import qualified Grep (match)
import Regexp(Regexp(Literal, Or), matchHere)

tests = [("xy", "123xy456", Just "xy"),
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
                
runTests :: [(String, String, Maybe String)] -> [(String, String, Maybe String)]
runTests [] = []
runTests ((regexp, string, expected):tests)
  | actual == expected = runTests tests
  | otherwise = (regexp, string, foldl concatMaybeString Nothing
                                 [Just "expected: '", 
                                  expected, Just "', actual: '", actual, Just "'"]):runTests tests
                where actual = Grep.match regexp string
                      
concatMaybeString :: Maybe String -> Maybe String -> Maybe String
concatMaybeString (Just s1) (Just s2) = Just (s1 ++ s2)
concatMaybeString (Just s1) Nothing = Just s1
concatMaybeString Nothing (Just s2) = Just s2
concatMaybeString Nothing Nothing = Nothing
                                            

matchHereTests :: [(Regexp, String, [(String, String)])]
matchHereTests = [(Literal "abc", "", []),
                  (Literal "x", "xbc", [("x", "bc")]),
                  ((Or (Literal "x") (Literal ("y")), "abc", [])),
                  ((Or (Literal "x") (Literal ("y")), "xbc", [("x", "bc")]))]
                 
runNewTests = runT matchHere matchHereTests

runT :: (Regexp -> String -> [(String, String)]) -> 
        [(Regexp, String, [(String, String)])] -> 
        [(Regexp, String, [(String, String)], [(String, String)])]
runT f [] = []
runT f ((r, t, expected):ss)
  | actual == expected = runT f ss
  | otherwise = (r, t, actual, expected): runT f ss
    where actual = f r t

main :: IO ()
main = do
        print $ runTests tests 
        print runNewTests 