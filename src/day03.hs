{-|
=Module          : Day 03 - Advent of Code
-Descrição       : Haskell module to solve day 03 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day03 where

import System.IO  
import Data.List
import Data.Char

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "../data/day03.txt" ReadMode
        contents <- hGetContents file 
        let lines = words contents
            rucksacks = map (\x -> splitAt (div (length x) 2) x) lines
            oddOnes = map (\(x,y) -> checkValue $ head $ intersect x y) rucksacks
            groups = elfGroups lines 
            badges = map (\x -> checkValue $ head $ getBadge x) groups
        print "Star 1:"
        print (sum oddOnes)
        print "Start 2:"
        print (sum badges)
        hClose file 

{-|
==== checkValue:
Given a character returns their value according to the rules:
'a' to 'z' => 1  to 26
'A' to 'Z' => 27 to 52
-}
checkValue :: Char -> Int
checkValue x
    | value >= 97 = value - 96
    | otherwise = value - 38
    where value = ord x

{-|
==== elfGroups:
Groups elf rucksacks in groups of three.
-}
elfGroups :: [String] -> [[String]]
elfGroups [] = []
elfGroups l = fst tuple : elfGroups (snd tuple) 
    where tuple = splitAt 3 l

{-|
==== getBadge:
Returns the common item from three rucksacks.
-}
getBadge :: [String] -> String
getBadge (x:y:z:xs) = intersect (intersect x y) (intersect y z)