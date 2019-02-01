-- Student information:
--  Student 1
--    Pereira
--    s
--  Student 2
--    Jonkman
--    s1563599

module CCenterOfMass where

import Clash.Prelude
import Image

-----------------------------------------------------------------------------------------
-- Assignment 4, Changing a pixel in a picture
-----------------------------------------------------------------------------------------
-- the changePixelInImage function takes an image, and produces a new image with a value replaced.
{-
changePixelInImage :: (KnownNat n1, KnownNat n2)
  => Vec n1 (Vec n1 Pixel) -- list of list of values
  -> SNat n2 -- row
  -> SNat n2 -- column
  -> Pixel   -- new value
  -> Vec n1 (Vec n1 Pixel) -- list of list of values with updated value
-- "select f s n xs" selects n elements with step-size s and offset f from xs.
changePixelInImage image y x p = ((select d0 d1 y image) ++ ((replace x p (at y image)):>Nil) ++ (select (y+1) d1 (subSNat (subSNat (lengthS image) d1) y) image))
-}


changePixelInImage :: (KnownNat n1, KnownNat n2) 
  => Vec n1 (Vec n2 Pixel) 
  -> Int 
  -> Int 
  -> Pixel 
  -> Vec n1 (Vec n2 Pixel)
changePixelInImage i row col c = replace k (replace l c (i!!k)) i
  where 
    k = row - 1
    l = col - 1

-----------------------------------------------------------------------------------------
-- Assignment 5, Center of mass of entire image
-----------------------------------------------------------------------------------------
threshold :: (KnownNat n1, KnownNat n2) 
  => Pixel 
  -> Vec n1 (Vec n2 Pixel)
  -> Vec n1 (Vec n2 Pixel)
threshold t i = map f i
  where f =  map (\x -> if (x > t) then 1 else 0)

-- Every list in the image represents a row (see printimage to check)
-- Calculates the row weight of every row in an image
wRows :: (KnownNat n1, KnownNat n2, Num a) 
  => Vec n1 (Vec n2 Pixel) 
  -> Vec n1 a
wRows i = map (foldl (+) 0) k 
  where k = map (map fromIntegral) i

comRows :: (KnownNat n1, KnownNat n2, Num a)
  => Vec n1 (Vec n2 Pixel)
  -> a
comRows i = fromIntegral (if j > 0 then div q j else 1)
  where 
    q = foldl (+) 0 (zipWith (*) (wRows i) (iterate (lengthS i) (+1) 1))
    j = toInteger (foldl (+) 0 (wRows i))

comCols :: (KnownNat n1, KnownNat n2, Num a)
  => Vec n1 (Vec n2 Pixel)
  -> a
comCols i = comRows (transpose i)

com :: (KnownNat n1, KnownNat n2, Num a)
  => Vec n1 (Vec n2 Pixel)
  -> (a, a)
com i = (comCols i, comRows i)

imageWithCom :: (KnownNat n1, KnownNat n2)
  => Pixel
  -> Vec n1 (Vec n2 Pixel)
  -> Vec n1 (Vec n2 Pixel)
imageWithCom c i = changePixelInImage k (comRows k) (comCols k) c
  where k = threshold 128 i

-----------------------------------------------------------------------------------------
-- Assignment 6 Center of mass of partsq =  of the image, with and without borders
-----------------------------------------------------------------------------------------
comParts i = unblocks2D (lengthS i) k
  where k = map (imageWithCom 2) (blocks2D d8 i)

comPartsWB i =  head (addBorders 2 ((comParts i):>Nil))

-----------------------------------------------------------------------------------------
-- Assignment 7 Time-area trade-off
-----------------------------------------------------------------------------------------


topEntity :: Vec 8 (Vec 8 Pixel) -> Vec 8 (Vec 8 Pixel)
topEntity = (imageWithCom 2)

------
-- TESTING
----
testVector = 
  (0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>
  (0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>(0:>0:>0:>0:>0:>0:>0:>0:>Nil):>Nil

test :: (KnownNat n1, KnownNat n2) => Vec n1 (Vec n2 Pixel) -> Int -> Int -> Pixel -> Vec n1 (Vec n2 Pixel)
test i row col c = replace row (replace col c (i!!row)) i

--test2 :: Integer -> Int
--test2 b = (b :: Int)
