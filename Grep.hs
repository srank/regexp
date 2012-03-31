module Grep (match) where

getMatchHere :: String -> String -> Maybe String
getMatchHere (r:rs) (x:xs)
 | r `elem` [x, '.'] = f x result
 | otherwise = Nothing
   where result = getMatchHere rs xs
         f :: Char -> Maybe String -> Maybe String
         f x Nothing = Nothing
         f x (Just xs) = Just (x:xs)
getMatchHere [] _ = Just []
getMatchHere _ [] = Nothing

match :: String -> String -> Maybe String
match rs (x:xs)
 | isMatch here = here
 | otherwise = match rs xs                                  
                            where here = getMatchHere rs (x:xs)   
                                  isMatch (Just _) = True
                                  isMatch _ = False
match rs [] = Nothing                          
   
testMatchHere = match "xy" "123xy456" == Just "xy"
                && match "x.y" "xzy" == Just "xzy"