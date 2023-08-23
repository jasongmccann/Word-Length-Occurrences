-- Jason McCann
-- wordLengths.hs
-- July 6 2020

-- Imports

import System.IO
import Data.List

-- Main Function

main = do
  wordFile <- readFile "english.sorted"
  let wordList = lines wordFile
  let minLength = minimum (map length wordList)
  let maxLength = maximum (map length wordList)
  let wordLengthGroups = [[word | word <- wordList, length word == len] | len <- [minLength .. maxLength]] 
  let wordLengthRange = setRangeOfLengths minLength maxLength
  let wordLengths = countOccurrences wordLengthGroups
  let wordExamples = pickExamples wordLengthGroups
  let wordExamplesString = examplesAsString wordExamples
  let wordLengthRangeString = integersAsString wordLengthRange
  let wordLengthsString = integersAsString wordLengths
  putStrLn "Word length     Occurrences     Sample words"
  putStrLn ""
  getwordLengths wordLengthRangeString wordLengthsString wordExamplesString
  
-- Functions 
 
setRangeOfLengths :: Int -> Int -> [Int] 
setRangeOfLengths min max 
 | min <= max = min : setRangeOfLengths (min + 1) max
 | otherwise = [] 
 
countOccurrences [] = []
countOccurrences (x:xs) = length x : countOccurrences xs

pickExamples [] = []
pickExamples (x:xs)
 | length x < 2 =  x: pickExamples xs
 | otherwise = take 2 x : pickExamples xs

getwordLengths [] [] []  = putStrLn ""
getwordLengths (x:xs) (y:ys) (z:zs) = do
                    putStr  x
                    putStr  "\t\t"
                    putStr  y 
                    putStr  "\t\t"
                    putStrLn  z
                    getwordLengths xs ys zs

examplesAsString [] = []
examplesAsString (x:xs) = intercalate ", " x : examplesAsString xs

integersAsString [] = []
integersAsString (x:xs) = show x : integersAsString xs
