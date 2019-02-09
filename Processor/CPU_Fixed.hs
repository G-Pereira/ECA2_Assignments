module CPU_Fixed where

import Clash.Prelude

-- Type synonyms
type Clk = Clock System Source
type Rst = Reset System Asynchronous
type Sig = Signal System
type Signed12 = Signed 12

-- Type declarations
data Opc = Add | Mul
  deriving (Eq, Show)

data Value = Stat Signed12 | Addr Signed12 | Top
  deriving (Eq, Show)

data Instr = Push Value | Calc Opc | Send Value | Pop
  deriving (Eq, Show)

program = 
  Push (Stat 2) :> Push (Addr 0) :> Pop :> Calc Mul :> Push (Stat 3) :>
  Push (Stat 4) :> Push (Addr 1) :> Pop :> Calc Add :> Pop :> Calc Mul :> Pop :>
  Calc Add :> Push (Stat 12) :> Push (Stat 5) :> Pop :> Calc Add :> Pop :>
  Calc Mul :> Send Top :> Push (Stat 2) :> Send Top :> Pop :> Send Top :> Nil

value h (s, sp) v = case v of
  Stat c -> c
  Addr addr -> h!!addr
  Top -> s!!sp

alu :: (Num a)
  => Opc
  -> a
  -> a
  -> a
alu op x y
  | op == Add = x + y
  | op == Mul = x * y
  | otherwise = 0

core :: (KnownNat n1, KnownNat n2, KnownNat n3) 
  => (Vec n1 Instr, Signed12, Signed12)
  -> (Signed12, (Vec n2 Signed12, Signed12), Vec n3 Signed12, Signed12)
  -> Bool
  -> ((Signed12, (Vec n2 Signed12, Signed12), Vec n3 Signed12, Signed12), Signed12)
core (prog, ssize, hsize) (pc, (stack, spntr), heap, reg) tick = ((pc', (stack', spntr'), heap', reg'), out)
  where
    heap' = heap 
    stack' = case instr of
      Push v    -> replace (spntr') (value heap (stack, spntr) v) stack
      Calc a    -> replace (spntr') (alu a reg (stack!!spntr)) stack
      otherwise -> stack
    spntr' = case instr of
      Push v    -> if spntr == 0 then ssize - 1 else spntr - 1
      Pop       -> mod (spntr + 1) ssize
      otherwise -> spntr
    reg' = case instr of
      Pop       -> stack!!spntr
      otherwise -> reg
    pc' = if tick == True then pc + 1 else pc
    out = case instr of
      Send v    -> stack'!!spntr'
      otherwise -> -1
    instr = prog!!pc

coreMealy en = mealy (core (program, ssize, hsize)) (0, (stackVector, 0), heapVector, -1) en
  where
    ssize = 4
    hsize = 2
    stackVector = replicate d10 0
    heapVector =  10:>11:>Nil

topEntity :: Clk -> Rst -> Sig Bool -> Sig Signed12
topEntity clk rst en = exposeClockReset coreMealy clk rst en
