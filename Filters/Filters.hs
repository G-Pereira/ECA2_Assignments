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
--    lastname:
--    student number:

-- NOTE: topentity functions are at the bottom
-- comment and uncomment the functions for each assignment
-- it should not be necessary, but you may change the 
-- definition of the topEntity functions.

-----------------------------------------------------------
-- Assignment 1
-- FIR1 N = 6 
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100 
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

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
--topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
--topEntity clk rst = exposeClockReset fir3_6 clk rst

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
