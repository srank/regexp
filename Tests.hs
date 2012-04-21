module Tests (main) where

import TokeniseTests(tokeniserTests)
import ParseTests
import MatchTests(matcHereTestResults, matchTestResults)


main :: IO ()
main = do
        print matcHereTestResults
        print matchTestResults 
        print tokeniserTests 
        print parseTestResults
