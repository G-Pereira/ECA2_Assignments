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

type Value_6 = Signed 8
fir1_6 :: Vec 6 Value_6 -> Vec 6 Value_6 -> Value_6
fir1_6 x h = foldl(+) 0 (zipWith(*) x h)

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100 
-----------------------------------------------------------

type Value_100 = SFixed 5 13
fir1_100 :: Vec 100 Value_100 -> Value_100
fir1_100 x = foldl(+) 0 (zipWith(*) x filterCoef)

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

type Value2_6 = Signed 8
fir2_6_f :: Vec 6 Value2_6 -> Value2_6 -> (Vec 6 Value2_6, Value2_6)
fir2_6_f u x = (u', z) where
    u' = x +>> u
    z = foldl(+) 0 (zipWith(*) u (1:>2:>3:>4:>5:>6:>Nil))

fir2_6 = mealy fir2_6_f (repeat 0)

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

type Value2_100 = SFixed 5 13
fir2_100_f :: Vec 100 Value2_100 -> Value2_100 -> (Vec 100 Value2_100, Value2_100)
fir2_100_f u x = (u', z) where
    u' = x +>> u
    z = foldl (+) 0 (zipWith (*) u filterCoef)

fir2_100 = mealy fir2_100_f inputSignal

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

type Value3_6 = Signed 8

fir3_6_f :: Vec 6 Value3_6 -> Value3_6 -> (Vec 6 Value3_6, Value3_6)
fir3_6_f u x = (u', z) where
    u' = x +>> u
    h = (1 :> 2 :> 3 :> Nil)
    w = foldl (+) 0 (zipWith (*) (take d3 u) h)
    z = foldl (+) w (zipWith (*) (take d3 (reverse u))  h)

fir3_6 = mealy fir3_6_f (repeat 0)

-----------------------------------------------------------
-- Assignment 6
-- FIR3 N = 100
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 7
-- FIR3' N = 6
-----------------------------------------------------------

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
--topEntity = fir1_6

-- Assignment 2
--topEntity = fir1_100

-- Assignment 3
--topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
--topEntity clk rst = exposeClockReset fir2_6 clk rst

-- Assignment 4
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset fir2_100 clk rst

-- Assignment 5
topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
topEntity clk rst = exposeClockReset fir3_6 clk rst

-- Assignment 6
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset fir3_100 clk rst

-- Assignment 7
--topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
--topEntity clk rst = exposeClockReset fir3t_6 clk rst

-- Assignment 8
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset iir1 clk rst

-- Assignment 9
--topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
--topEntity clk rst = exposeClockReset iir2 clk rst
