-- Student information:
--  Student 1
--    Pereira
--    s
--  Student 2
--    Jonkman
--    s1563599

module HCenterOfMass where

import Image
import Data.List

-----------------------------------------------------------------------------------------
-- Assignment 1, From grayscale to black and white
-----------------------------------------------------------------------------------------
thresholdHelper :: (Eq a, Num a, Ord a) => a -> [a] -> [a]
thresholdHelper _ [] = []
thresholdHelper t (x:xs) = if (x > t)
  then  1 : thresholdHelper t xs
  else  0 : thresholdHelper t xs

threshold :: (Eq a, Num a, Ord a) => a -> [[a]] -> [[a]]
threshold _ [] = [];
threshold t (i:is) = thresholdHelper t i : threshold t is

-----------------------------------------------------------------------------------------
-- Assignment 2, Center of mass of rows and picture
-----------------------------------------------------------------------------------------
-- Every list in the image represents a row (see printimage to check)

-- Calculates the row weight of every row in an image
wRows :: [[Int]] -> [Int]
wRows [] = []
wRows (is:iss) = foldl (+) 0 is : wRows iss

comRows :: [[Int]] -> Int
comRows i = div (foldl (+) 0 (zipWith (*) (wRows i) [1..(length i)])) k
  where 
    j = (foldl (+) 0 (wRows i))
    k = if j > 0 then j else 1

comCols i = comRows (transpose i)

com :: [[Int]] -> (Int, Int)
com i = (comCols i, comRows i)

imageWithCom c i = changePixelInImage k (comRows k) (comCols k) c
  where k = threshold 128 i

-----------------------------------------------------------------------------------------
-- Assignment 3 Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------
comPartsHelper [] = []
comPartsHelper (is:iss) = imageWithCom 2 is : comPartsHelper iss

comParts :: [[Int]] ->  [[Int]]
comParts i = unblocks2D (length i) (comPartsHelper (blocks2D 8 i))

comPartsWB :: [[Int]] ->  [[Int]]
comPartsWB i =  head (addBorders 2 [comParts i])
