module Filters where

import Clash.Prelude
import FilterCoefAndInput

type Clk = Clock System Source
type Rst = Reset System Asynchronous
type Sig = Signal System

-- Student information:
--  Student 1
--    lastname: Pereira
--    student number: s2203731
--  Student 2
--    lastname: Jonkman
--    student number: s1563599

-- NOTE: topentity functions are at the bottom
-- comment and uncomment the functions for each assignment
-- it should not be necessary, but you may change the 
-- definition of the topEntity functions.

-----------------------------------------------------------
-- Assignment 1
-- FIR1 N = 6 
-----------------------------------------------------------

fir h x = foldl (+) 0 (zipWith (*) x h)
fir1_6 = fir

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100 
-----------------------------------------------------------

fir1_100 = fir filterCoef

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

fir_reg h u x = (u', z) where
    u' = x +>> u
    z = fir h u

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

-- Uses same structure as Assignment 3 (fir_reg)
-- Check the modified call at the bottom

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

fir_sym h u x = (u', z) where
    u' = x +>> u
    w = zipWith (+) (takeI u) (takeI (reverse u))
    z = fir h w

-----------------------------------------------------------
-- Assignment 6
-- FIR3 N = 100
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 7
-- FIR3' N = 6
-----------------------------------------------------------


-- this would work for non symetrical (regular) coeficients 
fir_sym_t h u x = (u', z) where
    w = map (*x) h
    u'' = zipWith (+) w h
    u' = init u''
    z = last u''

-----------------------------------------------------------
-- Assignment 8
-- IIR
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 9
-- IIR
-----------------------------------------------------------

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------
-- Assignment 1
-- type Value = Signed 8
-- type Vector = Vec 6 Value

-- topEntity :: Vector -> Vector -> Value
-- topEntity = fir1_6

-- Assignment 2
-- type Value = SFixed 5 13
-- type Vector = Vec 100 Value

-- topEntity :: Vector -> Value
-- topEntity = fir1_100

-- Assignment 3
-- type Value = Signed 8
-- type Vector = Vec 6 Value
-- fir_reg :: Vector -> Vector -> Value -> (Vector, Value)

-- fir2_6 = mealy (fir_reg (1:>2:>3:>3:>2:>1:>Nil)) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
-- topEntity clk rst = exposeClockReset fir2_6 clk rst

-- Assignment 4
-- type Value = SFixed 5 13
-- type Vector = Vec 100 Value
-- fir_reg :: Vector -> Vector -> Value -> (Vector, Value)

-- fir2_100 = mealy (fir_reg filterCoef) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
-- topEntity clk rst = exposeClockReset fir2_100 clk rst

-- Assignment 5
-- type Value = Signed 8
-- type Vector = Vec 6 Value
-- type Vector_half = Vec 3 Value

-- fir_sym :: Vector_half -> Vector -> Value -> (Vector, Value)

-- fir3_6 = mealy (fir_sym (1:>2:>3:>Nil)) (repeat 0)

--topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
--topEntity clk rst = exposeClockReset fir3_6 clk rst

-- Assignment 6
-- type Value = SFixed 5 13
-- type Vector = Vec 100 Value
-- type Vector_half = Vec 50 Value

-- fir_sym :: Vector_half -> Vector -> Value -> (Vector, Value)

-- fir3_100 = mealy (fir_sym (takeI filterCoef)) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
-- topEntity clk rst = exposeClockReset fir3_100 clk rst

-- Assignment 7
type Value = Signed 8
type Vector = Vec 6 Value
type Vector_half = Vec 3 Value

fir_sym_t :: Vector_half -> Vector -> Value -> (Vector, Value)

fir3t_6 = mealy (fir_sym_t (1:>2:>3:>Nil)) (repeat 0)

topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
topEntity clk rst = exposeClockReset fir3t_6 clk rst

-- Assignment 8
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset iir1 clk rst

-- Assignment 9
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset iir2 clk rst
