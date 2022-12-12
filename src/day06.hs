{-|
=Module          : Day 06 - Advent of Code
-Descrição       : Haskell module to solve day 06 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day06 where

import System.IO
import Utils

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "../data/day06.txt" ReadMode
        contents <- hGetContents file 
        let pos = getSOP "" contents
        print "Star 1:"
        print pos
        print "Start 2:"
        --print (map last final9001)
        hClose file 

getSOP :: String -> String -> Int
getSOP x (y:ys)
    | (length x)<3 = 1+(getSOP (x++[y]) ys)
    | elem y x = 1+(getSOP ((tail x)++[y]) ys)
    | otherwise = 1