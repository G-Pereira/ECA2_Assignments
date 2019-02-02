module CCenterOfMass where

import Clash.Prelude
import Image

type Clk = Clock System Source
type Rst = Reset System Asynchronous
type Sig = Signal System

type Value = Unsigned 3

-- Student information:
--  Student 1
--    Pereira
--    s
--  Student 2
--    Jonkman
--    s1563599

-- Test Vectors
blackWhiteImage = threshold 128 image
imagePart = head (blocks2D d8 blackWhiteImage)
testVector = toList (concat (map (replicate d2) (blocks2D d8 blackWhiteImage)))

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



changePixelInImage :: (KnownNat n1, KnownNat n2, Enum a, Num a) 
  => Vec n1 (Vec n2 Pixel) 
  -> a 
  -> a 
  -> Pixel 
  -> Vec n1 (Vec n2 Pixel)
changePixelInImage i row col c = replace k (replace l c (i!!k)) i
  where 
    k = fromEnum (row - 1)
    l = fromEnum (col - 1)

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
com i = (comRows i, comCols i)

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

comPartsWB s i =  head (addBorders 2 ((comParts i):>Nil))

-----------------------------------------------------------------------------------------
-- Assignment 7 Time-area trade-off
-----------------------------------------------------------------------------------------
data StateCOM = StartCal | CalCOMRows | CalCOMCols

comTest :: (KnownNat n1, KnownNat n2, Num a, Num b1) 
  => (StateCOM, (a, b2))
  -> Vec n1 (Vec n2 Pixel) 
  -> ((StateCOM, (a, b1)), (a, b1))
comTest s i = (s', o)
  where 
    s' = case fst s of
      StartCal   -> (CalCOMCols, o)
      CalCOMRows -> (CalCOMCols, o)
      CalCOMCols -> (CalCOMRows, o)
    o = case fst s of
      StartCal   -> (0, 0)
      CalCOMRows -> (r, 0)
      CalCOMCols -> (fst (snd s), c)
    c = comCols i
    r = comRows i

mcom i = mealy comTest (StartCal, (0,0)) i

{-topEntity :: Vec 8 (Vec 8 Pixel) -> Vec 8 (Vec 8 Pixel)
topEntity = (imageWithCom 2)-}

topEntity ::
  Clk
  -> Rst
  -> Sig (Vec 8 (Vec 8 Pixel))  -- input image
  -> Sig (Value, Value) -- return
topEntity clk rst i = exposeClockReset mcom clk rst i
