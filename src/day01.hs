{-|
=Module          : Day 01 - Advent of Code
-Descrição       : Haskell module to solve day 01 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day01 where

import System.IO
import Data.List
import Utils

{-| 
= Main Function
-}
main = do  
        let list = []
        file <- openFile "../data/day01.txt" ReadMode
        contents <- hGetContents file 
        let elfStrs = splitBlankLines contents []
            elfCals = parseCals elfStrs []
            maxCal = maximum elfCals
            topCals = sum $ take 3 $ reverse $ sort elfCals
        print "Star 1:"
        print maxCal
        print "Start 2:"
        print topCals
        hClose file 

{-|
==== parseCals :
Converts a list of strings into a list with the calories carried by each Elf.
-}
parseCals :: [String] -> [Int] -> [Int]
parseCals [] l = l
parseCals (x:xs) l = parseCals xs (l++[sum $ toNumber $ words x])