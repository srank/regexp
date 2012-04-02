module Tests (main) where
import Grep (match)

testMatch = match "xy" "123xy456" == Just "xy"
                && match "x.y" "xzy" == Just "xzy"
                && match "^1234" "123456" == Just "1234"
                && match "12^abc" "abc" == Nothing
                && match "abc^" "abc" == Nothing
                && match "a*" "aaa" == Just "aaa"
                
main :: IO ()
main = print testMatch