module Polynomial where

import Clash.Prelude

-- type System is defined in 'Clash.Explicit.Signal' as: Dom "system" 10000
-- Hence System is a domain identified by 'system' and clock period 10ns
-- Ungated (due to Source) system (read domain) clock
type Clk = Clock System Source

-- Asynchronous reset for system domain
type Rst = Reset System Asynchronous
type Sig = Signal System

type Value = Signed 16
type Vector = Vec 3 Value

-- Student information:
--  Student 1
--    Pereira
--    s
--  Student 2
--    Jonkman
--    s1563599


-- NOTE: topentity functions are at the bottom
-- comment and uncomment the functions for each assignment

-----------------------------------------------------------
-- Assignment 1
-- f0p f0m
-----------------------------------------------------------
f0p :: Value -> Value
f0p x = x^4 + 22*x^3 + 172*x^2 + 552*x + 576

f0m :: Value -> Value
f0m x = x*x*x*x + 22*x*x*x + 172*x*x + 552*x + 576

f0mb :: Value -> Value
f0mb x = (x*x*x*x) + 22*(x*x*x) + 172*(x*x) + 552*x + 576
-----------------------------------------------------------
-- Assignment 2
-- f0f
-----------------------------------------------------------
f0f :: Value -> Value
f0f x = (x + 2) * (x + 6) * (x + 6) * (x + 8)
-----------------------------------------------------------
-- Assignment 3
-- f0hof
----------------------------------------------------------
helper x y z = y*(x+z)

--f0hof :: Value -> Value
f0hof x = foldl (helper x) ((2+x) :: Value) (6:>6:>8:>Nil)

-----------------------------------------------------------
-- Assignment 4
-- f0mealy
----------------------------------------------------------
f0helper s (x, y) = (s', s')
  where
    s'= s*(x+y)

f0mealy x = mealy f0helper 1 x

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------

-- Assignment 1
--topEntity = f0p

-- Assignment 2
--topEntity = f0f

-- Assignment 3
--topEntity = f0hof

-- Assignment 4
topEntity :: Clk -> Rst -> Sig (Value, Value) -> Sig Value
topEntity clk rst x = exposeClockReset f0mealy clk rst x
