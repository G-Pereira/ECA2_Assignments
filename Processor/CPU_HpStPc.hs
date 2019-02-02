module CPU_HpStPc where

-- import Clash.Prelude

data Opc = Add | Mul
  deriving (Eq, Show)

data Value = Const Int | Addr Int | Top
  deriving (Eq, Show)

data Instr = Push Value | Calc Opc | Send Value | Pop
  deriving (Eq, Show)

program = [Push (Const 2), Push (Addr 0), Calc Mul, Push (Const 3),
  Push (Const 4), Push (Addr 1), Calc Add, Calc Mul, Calc Add,
  Push (Const 12), Push (Const 5), Calc Add, Calc Mul, Send Top]

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
  -> (Int, [Int], [Int]) 
  -> a 
  -> ((Int, [Int], [Int]), Int)
core prog (pc, stack, heap) tick = ((pc', stack', heap'), out)
  where
    heap' = heap 
    stack' = case instr of
      Push v -> value heap stack v : stack
      Calc a -> alu a (stack!!0) (stack!!1) : drop 2 stack
      Pop -> tail stack
      otherwise -> stack
    pc' = if tick == 1 then pc + 1 else pc
    out = case instr of
      Send v -> value heap stack v
      otherwise -> -1
    instr = prog!!pc

-- Simulation functionality
sim f s [] = []
sim f s (x:xs) = z : sim f s' xs
  where
    (s', z) = f s x

-- Test with: test = sim core ([], [11, 10]) program
