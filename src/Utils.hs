module Utils where

{-|
==== splitStrBy:
Given a character and string splits in list of substrings separated by that char.
-}
splitStrBy :: Char -> String -> [String]
splitStrBy _ [] = []
splitStrBy c s = fst res : splitStrBy c (snd res)
    where res = buildPairs c ([],[]) s

{-|
==== splitStrAt:
Given a character and a string splits a string into a tuple before and after it.
-}
splitStrAt :: Char -> String -> (String,String)
splitStrAt c s = buildPairs c ([],[]) s

{-|
==== buildPairs:
Given a character a pair and a string adds to the first element of the pair all
the chars before the desired character, adding the others to the snd one. 
-}
buildPairs :: Char -> (String,String) -> String -> (String,String)
buildPairs _ r [] = r
buildPairs c (r1,r2) (x:xs)
    |c==x = (r1,r2++xs)
    |otherwise = buildPairs c (r1++[x],r2) xs 


{-|
==== splitBlankLines :
Converts a list of characters into a list of strings separated by blank lines
-}
splitBlankLines :: [Char] -> [String] -> [String]
splitBlankLines [] y = y
splitBlankLines x [] = splitBlankLines x [[]]
splitBlankLines (x:xs) y 
    | x=='\n' && (last y)==[] = splitBlankLines xs y
    | x=='\n' && (last (last y))=='\n' = splitBlankLines xs (y++[[]])
    | otherwise = splitBlankLines xs ((init y)++[(last y)++[x]])

{-|
==== toNumber :
Converts list of strings to list of Integers.
-}
toNumber :: [String] -> [Int]
toNumber = map read