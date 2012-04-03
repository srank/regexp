module Tests (main) where
import Grep (match)

tests = [("xy", "123xy456", Just "xy"),
         ("x.y", "xzy", Just "xzy"),
         ("^1234", "123456", Just "1234"),
         ("12^abc", "abc", Nothing),
         ("abc^", "abc", Nothing),
         ("a*", "aaa", Just "aaa"),
         ("c*", "abccd", Just "cc"]
                
runTests :: [(String, String, Maybe String)] -> [(String, String, Maybe String)]
runTests [] = []
runTests ((regexp, string, expected):tests)
  | match regexp string == expected = runTests tests
  | otherwise = (regexp, string, expected):runTests tests
                                            
main :: IO ()
main = print $ runTests tests