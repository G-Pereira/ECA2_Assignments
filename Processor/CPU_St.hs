module CPU_St where

--import Clash.Prelude

data Opc = Add | Mul
  deriving (Eq, Show)

data Instr = Push Int | Calc Opc
  deriving (Eq, Show)

program = [Push 2, Push 10, Calc Mul, Push 3, Push 4, Push 11,
  Calc Add, Calc Mul, Calc Add, Push 12, Push 5, Calc Add, Calc Mul]

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
  [Int] 
  -> Instr 
  -> [Int]
core s i = case i of
  Push b -> b : s;
  Calc a -> alu a (s!!0) (s!!1) : drop 2 s
