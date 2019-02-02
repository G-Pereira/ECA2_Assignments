module CPU_HpStPcReg where

-- import Clash.Prelude

data Opc = Add | Mul
  deriving (Eq, Show)

data Value = Const Int | Addr Int | Top
  deriving (Eq, Show)

data Instr = Push Value | Calc Opc | Send Value | Pop
  deriving (Eq, Show)

program1 = 
  [
    Push (Const 2), Push (Addr 0), Calc Mul, Push (Const 3),
    Push (Const 4), Push (Addr 1), Calc Add, Calc Mul, Calc Add,
    Push (Const 12), Push (Const 5), Calc Add, Calc Mul, Send Top
  ]

program2 = 
  [
    Push (Const 2), Push (Addr 0), Pop, Calc Mul, Push (Const 3),
    Push (Const 4), Push (Addr 1), Pop, Calc Add, Pop, Calc Mul, Pop,
    Calc Add, Push (Const 12), Push (Const 5), Pop, Calc Add, Pop,
    Calc Mul, Send Top, Push (Const 2), Send Top, Pop, Send Top
  ]

value :: 
  [Int] 
  -> [Int]
  -> Value 
  -> Int
value h s v = case v of
  Const c -> c
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

core :: (Eq a, Num a) 
  => [Instr] 
  -> (Int, [Int], [Int], (Int, Bool)) 
  -> a 
  -> ((Int, [Int], [Int], (Int, Bool)), Int)
core prog (pc, stack, heap, (reg, valid)) tick = ((pc', stack', heap', (reg', valid')), out)
  where
    heap' = heap 
    stack' = case instr of
      Push v -> value heap stack v : stack
      Calc a -> if valid == True then alu a reg (stack!!0) : tail stack else tail stack
      Pop -> tail stack
      otherwise -> stack
    reg' = case instr of
      Calc a -> if valid == True then -1 else (stack!!0)
      otherwise -> -1
    valid' = case instr of
      Calc a -> not valid
      otherwise -> False
    pc' = 
      if tick == 1 
        then
          case instr of
            Calc a -> 
              if valid == True 
                then pc + 1 
                else pc
            otherwise -> pc + 1 
        else pc
    out = case instr of
      Send v -> value heap stack v
      otherwise -> -1
    instr = prog!!pc

-- Simulation functionality
sim f s [] = []
sim f s (x:xs) = z : sim f s' xs
  where
    (s', z) = f s x

-- Test with: test = sim (core program1) (0, [], [10, 11], (-1, False)) $ repeat 1
-- We believe program 2 to be invalid as it pops an element in the third instruction, 
-- yielding an invalid multiplication
