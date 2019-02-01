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

fir h x = foldl (+) 0 (zipWith (*) x h)
fir1_6 = fir

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100 
-----------------------------------------------------------

coef = (0.00641067965727758:> 0.00638175172557373:> 0.00619115765981477:> 0.00583664072411354:> 0.00531966453546420:> 0.00464552212014080:> 0.00382336686588339:> 0.00286616281883657:> 0.00179055342648450:> 0.000616649526696630:> -0.000632260907119334:> -0.00193007707010667:> -0.00324831786134811:> -0.00455661530591583:> -0.00582326012243003:> -0.00701578945981511:> -0.00810160578123223:> -0.00904861504126774:> -0.00982587170445480:> -0.0104042178044341:> -0.0107569031537370:> -0.0108601739891343:> -0.0106938177760437:> -0.0102416525913690:> -0.00949195044563220:> -0.00843778507535779:> -0.00707729611344373:> -0.00541386310224685:> -0.00345618452088869:> -0.00121825882103480:> 0.00128073363236992:> 0.00401664687562740:> 0.00696068113146024:> 0.0100797853736926:> 0.0133371441513720:> 0.0166927404388704:> 0.0201039847163911:> 0.0235263991072961:> 0.0269143442339414:> 0.0302217755271657:> 0.0334030150563411:> 0.0364135245518031:> 0.0392106651788269:> 0.0417544297955815:> 0.0440081338842948:> 0.0459390520769479:> 0.0475189881902650:> 0.0487247679202216:> 0.0495386447994046:> 0.0499486116624269:> 0.0499486116624269:> 0.0495386447994046:> 0.0487247679202216:> 0.0475189881902650:> 0.0459390520769479:> 0.0440081338842948:> 0.0417544297955815:> 0.0392106651788269:> 0.0364135245518031:> 0.0334030150563411:> 0.0302217755271657:> 0.0269143442339414:> 0.0235263991072961:> 0.0201039847163911:> 0.0166927404388704:> 0.0133371441513720:> 0.0100797853736926:> 0.00696068113146024:> 0.00401664687562740:> 0.00128073363236992:> -0.00121825882103480:> -0.00345618452088869:> -0.00541386310224685:> -0.00707729611344373:> -0.00843778507535779:> -0.00949195044563220:> -0.0102416525913690:> -0.0106938177760437:> -0.0108601739891343:> -0.0107569031537370:> -0.0104042178044341:> -0.00982587170445480:> -0.00904861504126774:> -0.00810160578123223:> -0.00701578945981511:> -0.00582326012243003:> -0.00455661530591583:> -0.00324831786134811:> -0.00193007707010667:> -0.000632260907119334:> 0.000616649526696630:> 0.00179055342648450:> 0.00286616281883657:> 0.00382336686588339:> 0.00464552212014080:> 0.00531966453546420:> 0.00583664072411354:> 0.00619115765981477:> 0.00638175172557373:> 0.00641067965727758 :> Nil)

fir1_100 = fir coef

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

fir_reg h u x = (u', z) where
    u' = x +>> u
    z = fir h u

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

-- Uses same structure as Assignment 3 (fir_reg)
-- Check the modified call at the bottom

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

fir3_6_f h u x = (u', z) where
    u' = x +>> u
    w = zipWith (+) (take d3 u) (take d3 (reverse u))
    z = fir h w

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
-- type Value = Signed 8
-- type Vector = Vec 6 Value

-- topEntity :: Vector -> Vector -> Value
-- topEntity = fir1_6

-- Assignment 2
-- type Value = SFixed 5 13
-- type Vector = Vec 100 Value

-- topEntity :: Vector -> Value
-- topEntity = fir1_100

-- Assignment 3
-- type Value = Signed 8
-- type Vector = Vec 6 Value
-- fir_reg :: Vector -> Vector -> Value -> (Vector, Value)

-- fir2_6 = mealy (fir_reg (1:>2:>3:>3:>2:>1:>Nil)) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
-- topEntity clk rst = exposeClockReset fir2_6 clk rst

-- Assignment 4
-- type Value = SFixed 5 13
-- type Vector = Vec 100 Value
-- fir_reg :: Vector -> Vector -> Value -> (Vector, Value)

-- fir2_100 = mealy (fir_reg filterCoef) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
-- topEntity clk rst = exposeClockReset fir2_100 clk rst

-- Assignment 5
-- type Value = Signed 8
-- type Vector = Vec 6 Value
-- type Vector_half = Vec 3 Value
-- fir3_6_f :: Vector_half -> Vector -> Value -> (Vector, Value)

-- fir3_6 = mealy (fir3_6_f (1:>2:>3:>Nil)) (repeat 0)

-- topEntity :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
-- topEntity clk rst = exposeClockReset fir3_6 clk rst

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
