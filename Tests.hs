module Tests (main) where
import Grep (match)

testMatch = match "xy" "123xy456" == Just "xy"
                && match "x.y" "xzy" == Just "xzy"
                
main :: IO ()
main = print testMatch