module Grep (match) where

-- Match the (restricted) regexp r:rs against the string x:xs
-- 

getMatchHere :: String -> String -> Maybe String
getMatchHere (r:rs) (x:xs)
 | r `elem` [x, '.'] = appendMatch x result
 | otherwise = Nothing
   where result = getMatchHere rs xs
         appendMatch :: Char -> Maybe String -> Maybe String
         appendMatch x Nothing = Nothing
         appendMatch x (Just xs) = Just (x:xs)
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
