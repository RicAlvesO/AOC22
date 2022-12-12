{-|
=Module          : Day 05 - Advent of Code
-Descrição       : Haskell module to solve day 05 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day05 where

import System.IO
import Utils

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "../data/day05.txt" ReadMode
        contents <- hGetContents file 
        let parts = splitBlankLines contents []
            stacks = splitStrBy '\n' (head parts)
            commands = map (\x -> toNumber [x!!1,x!!3,x!!5]) $  map (splitStrBy ' ') $ splitStrBy '\n' $ last parts
            final9000 = crateMover9000 stacks commands
            final9001 = crateMover9001 stacks commands
        print "Star 1:"
        print (map last final9000)
        print "Start 2:"
        print (map last final9001)
        hClose file 

crateMover9000 :: [String] -> [[Int]] -> [String]
crateMover9000 s [] = s
crateMover9000 s ((a:f:t:ys):xs) = crateMover9000 (replace (f-1) (replace (t-1) s rt) rf) xs
    where res = reverse $ drop (length (s!!(f-1)) - a) (s!!(f-1))
          rf  = take (length (s!!(f-1)) - a) (s!!(f-1))
          rt  = (s!!(t-1)) ++ res

crateMover9001 :: [String] -> [[Int]] -> [String]
crateMover9001 s [] = s
crateMover9001 s ((a:f:t:ys):xs) = crateMover9001 (replace (f-1) (replace (t-1) s rt) rf) xs
    where res = drop (length (s!!(f-1)) - a) (s!!(f-1))
          rf  = take (length (s!!(f-1)) - a) (s!!(f-1))
          rt  = (s!!(t-1)) ++ res