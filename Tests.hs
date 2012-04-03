module Tests (main) where
import Grep (match)

tests = [("xy", "123xy456", Just "xy"),
         ("z", "xyz", Just "z"),
         ("x.y", "xzy", Just "xzy"),
         ("^1234", "123456", Just "1234"),
         ("12^abc", "abc", Nothing),
         ("abc^", "abc", Nothing),
         ("a*", "aaa", Just "aaa"),
         ("c*", "abccd", Just "cc")]
                
runTests :: [(String, String, Maybe String)] -> [(String, String, Maybe String)]
runTests [] = []
runTests ((regexp, string, expected):tests)
  | actual == expected = runTests tests
  | otherwise = (regexp, string, foldl concatMaybeString Nothing
                                 [Just "expected: '", 
                                  expected, Just "', actual: '", actual, Just "'"]):runTests tests
                where actual = match regexp string
                      
concatMaybeString :: Maybe String -> Maybe String -> Maybe String
concatMaybeString (Just s1) (Just s2) = Just (s1 ++ s2)
concatMaybeString (Just s1) Nothing = Just s1
concatMaybeString Nothing (Just s2) = Just s2
concatMaybeString Nothing Nothing = Nothing
                                            
main :: IO ()
main = print $ runTests tests