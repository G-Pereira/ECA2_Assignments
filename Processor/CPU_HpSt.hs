module CPU_HpSt where

-- import Clash.Prelude

data Opc = Add | Mul
  deriving (Eq, Show)

data Value = Const Int | Addr Int
  deriving (Eq, Show)

data Instr = Push Value | Calc Opc
  deriving (Eq, Show)

program = [Push (Const 2), Push (Addr 0), Calc Mul, Push (Const 3),
  Push (Const 4), Push (Addr 1), Calc Add, Calc Mul, Calc Add,
  Push (Const 12), Push (Const 5), Calc Add, Calc Mul]

value :: 
  [Int] 
  -> Instr 
  -> Int
value h i = case i of
  Push b -> 
    case b of
      Const c -> c
      Addr addr -> h!!addr

alu :: (Num a)
  => Opc
  -> a
  -> a
  -> a
alu op x y
  | op == Add = x + y
  | op == Mul = x * y
  | otherwise = 0

core :: 
  ([Int], [Int]) 
  -> Instr 
  -> ([Int], [Int])
core (stack, heap) instr = (stack', heap') 
  where
    heap' = heap 
    stack' = case instr of
      Push b -> value heap instr : stack;
      Calc a -> alu a (stack!!0) (stack!!1) : drop 2 stack
