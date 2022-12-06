{-|
=Module          : Day 02 - Advent of Code
-Descrição       : Haskell module to solve day 02 of AoC
-Desenvolvedores : Ricardo Oliveira <ricaoimp@gmail.com>
-}
module Day02 where

import System.IO  
import Control.Monad
import Data.List

{-| 
= Main Function 
-}
main = do  
        let list = []
        file <- openFile "data/day02.txt" ReadMode
        contents <- hGetContents file 
        let games = parseGames contents []
            scores = map checkScore games
            newScores = map checkShape games
        print "Star 1:"
        print (sum scores)
        print "Start 2:"
        print (sum newScores)
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
parseGames :: [Char] -> [(String,String)] -> [(String,String)]
parseGames [] l = l
parseGames a l = parseGames (snd b) l++[(game!!0,game!!1)] 
    where b=splitAt 4 a
          game=words (fst b)

{-|
==== checkScore :
Given a pair os strings calculates the score for the game according to the rules:
A & X => ROCK
B & Y => PAPER
C & Z => SCISORS
-}
checkScore :: (String,String) -> Int
checkScore ("A","X") = 3+1 
checkScore ("A","Y") = 6+2 
checkScore ("A","Z") = 0+3 
checkScore ("B","X") = 0+1 
checkScore ("B","Y") = 3+2 
checkScore ("B","Z") = 6+3 
checkScore ("C","X") = 6+1 
checkScore ("C","Y") = 0+2 
checkScore ("C","Z") = 3+3 

{-|
==== checkScore :
Given a pair os strings calculates the score for the game according to the rules:
X => LOSE
Y => DRAW
Z => WIN
-}
checkShape :: (String,String) -> Int
checkShape ("A","X") = 3+0
checkShape ("A","Y") = 1+3
checkShape ("A","Z") = 2+6
checkShape ("B","X") = 1+0
checkShape ("B","Y") = 2+3
checkShape ("B","Z") = 3+6
checkShape ("C","X") = 2+0
checkShape ("C","Y") = 3+3
checkShape ("C","Z") = 1+6