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

program1 =
    Push (Stat 2):> Push (Addr 0):> Calc Mul:> Send Top :> 
    Push (Stat 3):> Push (Stat 4):> Push (Addr 1):> Calc Add :> Send Top :> 
    Calc Mul :> Send Top :> 
    Calc Add :> Send Top :> 
    Push (Stat 12):> Push (Stat 5):> Calc Add :> Send Top :> 
    Calc Mul:> Send Top :> Nil

program2 = 
  [
    Push (Stat 2), Push (Addr 0), Pop, Calc Mul, Push (Stat 3),
    Push (Stat 4), Push (Addr 1), Pop, Calc Add, Pop, Calc Mul, Pop,
    Calc Add, Push (Stat 12), Push (Stat 5), Pop, Calc Add, Pop,
    Calc Mul, Send Top, Push (Stat 2), Send Top, Pop, Send Top
  ]

value :: (KnownNat n1, KnownNat n2) 
  => Vec n1 Signed12 
  -> Vec (n2 + 1) Signed12 
  -> Value 
  -> Signed12
value h s v = case v of
  Stat c -> c
  Addr addr -> h!!addr
  Top -> head s

alu :: (Num a)
  => Opc
  -> a
  -> a
  -> a
alu op x y
  | op == Add = x + y
  | op == Mul = x * y
  | otherwise = 0

core :: (KnownNat n1, KnownNat n2, KnownNat n3, Enum a, Num a) 
  => Vec n2 Instr
  -> (a, Vec (n1 + 1) Signed12, Vec n3 Signed12, (Signed12, Bool))
  -> Bool
  -> ((a, Vec (n1 + 1) Signed12, Vec n3 Signed12, (Signed12, Bool)), Signed12)
core prog (pc, stack, heap, (reg, valid)) en = ((pc', stack', heap', (reg', valid')), out)
  where
    heap' = heap 
    stack' = case instr of
      Push v -> (value heap stack v) +>> stack
      Calc a -> if valid == True then (replace 0 (alu a reg (stack!!0)) stack) else stack <<+ (last stack)
      Pop -> stack <<+ (last stack)
      otherwise -> stack
    reg' = case instr of
      Calc a -> if valid == True then -1 else (stack!!0)
      otherwise -> -1
    valid' = case instr of
      Calc a -> not valid
      otherwise -> False
    pc' = 
      case instr of
        Calc a -> 
          if valid == True 
            then pc + 1 
            else pc
        otherwise -> pc + 1
    out = case instr of
      Send v -> value heap stack v
      otherwise -> -1
    instr = prog!!pc

coreMealy en = mealy (core program1) (0, (replicate d40 0), 10:>11:>Nil ++ (replicate d38 0), (-1, False)) en

topEntity :: Clk -> Rst -> Sig Bool -> Sig Signed12
topEntity clk rst en = exposeClockReset coreMealy clk rst en

-- Test with: test = sim (core program1) (0, [], [10, 11], (-1, False)) $ repeat 1
-- We believe program 2 to be invalid as it pops an element in the third instruction, 
-- yielding an invalid multiplication
