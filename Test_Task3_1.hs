module Test_Task3_1 where

import           Task3_1

wpn = Succ $ Succ $ Pred $ Succ $ Succ $ Zero

wpnStr = show wpn
parsedWpn = read wpnStr :: WeirdPeanoNumber
showEqRead = wpnStr == show parsedWpn

test_eq = wpn == parsedWpn
test_neq = wpn /= Succ wpn

wpnFromInt i | i == 0 = Zero
             | i < 0  = peanoFromSuccPred 0 (-i)
             | i > 0  = peanoFromSuccPred i 0

five = wpnFromInt 5
mfive = wpnFromInt (-5)

test_le = mfive < five
test_ge = five > mfive

test_nle = not (five < mfive)
test_nge = not (mfive > five)

test_minus = five == (-mfive)
test_mult = [five * five == 25, (-five) * five == (-25), five * (-five)  == (-25), (-five) * (-five)  == 25]
