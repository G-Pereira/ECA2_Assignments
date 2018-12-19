module Polynomial where

import Clash.Prelude

type Clk = Clock System Source
type Rst = Reset System Asynchronous
type Sig = Signal System

type Value = Signed 16

-- Student information:
--  Student 1
--    lastname:
--    student number:
--  Student 2
--    lastname:
--    student number:


-- NOTE: topentity functions are at the bottom
-- comment and uncomment the functions for each assignment

-----------------------------------------------------------
-- Assignment 1
-- f0p f0m
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 2
-- f0f
-----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 3
-- f0hof
----------------------------------------------------------

-----------------------------------------------------------
-- Assignment 4
-- f0mealy
----------------------------------------------------------

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------

-- Assignment 1
--topEntity hs x = (f0m hs x, f0p hs x)

-- Assignment 2
--topEntity hs x = f0f hs x

-- Assignment 3
--topEntity hs x = f0hof hs x

-- Assignment 4
--topEntity :: Clk -> Rst -> Value -> Sig Value -> Sig Value
--topEntity clk rst x = exposeClockReset (f0mealy x) clk rst

