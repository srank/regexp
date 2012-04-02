module Grep (match) where

getMatch :: String -> String -> Maybe String
getMatch (r:rs) (x:xs)
 | r `elem` [x, '.'] = f x result
 | otherwise = Nothing
   where result = getMatch rs xs
         f :: Char -> Maybe String -> Maybe String
         f x Nothing = Nothing
         f x (Just xs) = Just (x:xs)
getMatch [] _ = Just []
getMatch _ [] = Nothing

match :: String -> String -> Maybe String
match rs (x:xs)
 | isMatch here = here
 | otherwise = match rs xs                                  
   where here = getMatch rs (x:xs)   
         isMatch (Just _) = True
         isMatch _ = False
match rs [] = Nothing                          
