{-|
=Module          : Day 03 - Advent of Code
-Descrição       : Haskell module to solve day 03 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day04 where

import System.IO
import Utils

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "../data/day04.txt" ReadMode
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

{-|
==== buildSections:
Given a pair of two strings representing a range between two numbers,
create a pair of the two lists of sections.
-}
buildSections :: (String,String) -> ([Int],[Int])
buildSections (s1,s2) = ([(read (fst r1)::Int)..(read (snd r1)::Int)],[(read (fst r2)::Int)..(read (snd r2)::Int)])
    where r1 = splitStrAt '-' s1
          r2 = splitStrAt '-' s2

{-|
==== intersections:
Check if one list is completely contained in another
-}
intersections :: [Int] -> [Int] -> Bool
intersections x y = (elem False (map (\a -> elem a y ) x))==False || (elem False (map (\b -> elem b x) y))==False

{-|
==== someIntersections:
Check if any elemente of a list is contained in the other
-}
someIntersections :: [Int] -> [Int] -> Bool
someIntersections x y = elem True (map (\a -> elem a y ) x)