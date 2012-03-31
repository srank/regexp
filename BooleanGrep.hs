module BooleanGrep (match) where

-- The following functions simply return whether a match was found,
-- not what the match was 

matchHere :: String -> String -> Bool
matchHere (r:rs) (x:xs)
 | r /= x = False
 | otherwise = matchHere rs xs
matchHere [] _ = True
matchHere (r:rs) [] = False

match :: String -> String -> Bool
match (r:rs) (x:xs)
 | r == x = matchHere rs xs
 | otherwise = match (r:rs) (xs)
match _ [] = False
