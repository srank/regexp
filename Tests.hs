module Tests (testMatch) where
import Grep

testMatch = match "xy" "123xy456" == Just "xy"
                && match "x.y" "xzy" == Just "xzy"