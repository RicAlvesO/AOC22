{-|
=Module          : Day 01 - Advent of Code
-Descrição       : Haskell module to solve day 01 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day01 where

import System.IO  
import Control.Monad
import Data.List

{-| 
= Main Function :
- Parses Input from file to list of Integers;
- Calls `dayOne` function with 1 to solve AoC1p1;
- Calls `dayOne` function with 3 to solve AoC1p2.
-}
main = do  
        let list = []
        file <- openFile "data/day01.txt" ReadMode
        contents <- hGetContents file 
        let elfStrs = parseContents contents [] []
            elfCals = parseCals elfStrs []
            maxCal = maximum elfCals
            topCals = sum $ take 3 $ reverse $ sort elfCals
        print "Star 1:"
        print maxCal
        print "Start 2:"
        print topCals
        hClose file 

{-|
==== toNumber :
Converts list of strings to list of Integers.
-}
toNumber :: [String] -> [Int]
toNumber = map read

{-|
==== parseContents :
Converts a list of characters into a list of strings separated by blank lines
-}
parseContents :: [Char] -> [Char] -> [String] -> [String]
parseContents [] _ l = l
parseContents (x:xs) [] l = parseContents xs [x] l
parseContents (x:xs) y l
    |x==(last y) && x=='\n' = parseContents xs [] (l++[y])
    |otherwise = parseContents xs (y++[x]) l 

{-|
==== parseCals :
Converts a list of strings into a list with the calories carried by each Elf.
-}
parseCals :: [String] -> [Int] -> [Int]
parseCals [] l = l
parseCals (x:xs) l = parseCals xs (l++[sum $ toNumber $ words x])