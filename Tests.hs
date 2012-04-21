module Tests(main) where

import TokeniseTests(tokeniserTests)
import ParseTests(parseTestResults)
import MatchTests(matchHereTestResults, matchTestResults)


main :: IO ()
main = do
        print matchHereTestResults
        print matchTestResults 
        print tokeniserTests 
        print parseTestResults
