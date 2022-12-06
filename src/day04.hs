{-|
=Module          : Day 03 - Advent of Code
-Descrição       : Haskell module to solve day 03 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day04 where

import System.IO  
import Control.Monad
import Data.List
import Data.Char

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "data/day04.txt" ReadMode
        contents <- hGetContents file 
        let lines = words contents
            sections = map (\x -> buildSections $ splitStrAt ',' x) lines
            containsAll = map (\(x,y) -> if intersections x y then 1 else 0) sections
            containsSome = map (\(x,y) -> if someIntersections x y then 1 else 0) sections
        print "Star 1:"
        print (sum containsAll)
        print "Start 2:"
        print (sum containsSome)
        hClose file 

splitStrAt :: Char -> String -> (String,String)
splitStrAt c l = buildPairs c ([],[]) l  

buildPairs :: Char -> (String,String) -> String -> (String,String)
buildPairs _ r [] = r
buildPairs c (r1,r2) (x:xs)
    |c==x = (r1,r2++xs)
    |otherwise = buildPairs c (r1++[x],r2) xs 

buildSections :: (String,String) -> ([Int],[Int])
buildSections (s1,s2) = ([(read (fst r1)::Int)..(read (snd r1)::Int)],[(read (fst r2)::Int)..(read (snd r2)::Int)])
    where r1 = splitStrAt '-' s1
          r2 = splitStrAt '-' s2

intersections :: [Int] -> [Int] -> Bool
intersections x y = (elem False (map (\a -> elem a y ) x))==False || (elem False (map (\b -> elem b x) y))==False

someIntersections :: [Int] -> [Int] -> Bool
someIntersections x y = elem True (map (\a -> elem a y ) x)