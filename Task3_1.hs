module Task3_1 where

import Todo(todo)

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
        show Zero = "Zero "
        show (Succ wpn) = "Succ " ++ show wpn
        show (Pred wpn) = "Pred " ++ show wpn


readWpn :: [Char] -> WeirdPeanoNumber
readWpn (wpn0 : wpn1 : wpn2 : wpn3 : ' ' : others) =
  case [wpn0, wpn1, wpn2, wpn3] of
    "Zero" -> Zero
    "Succ" -> Succ (readWpn others)
    "Pred" -> Pred (readWpn others)
    _      -> error "Not a WeirdPeanoNumber"

instance Read WeirdPeanoNumber where
        readsPrec _ str = [(readWpn str, "")]

peanoSequence
  :: (WeirdPeanoNumber -> WeirdPeanoNumber)
  -> Integer
  -> (WeirdPeanoNumber -> WeirdPeanoNumber)
peanoSequence wpn 0 = wpn
peanoSequence wpn i = wpn . peanoSequence wpn (i - 1)

countSuccPred :: WeirdPeanoNumber -> (Integer, Integer)
countSuccPred Zero       = (0, 0)
countSuccPred (Succ wpn) = let (sc, pd) = countSuccPred wpn in (sc + 1, pd)
countSuccPred (Pred wpn) = let (sc, pd) = countSuccPred wpn in (sc, pd + 1)

peanoFromSuccPred :: Integer -> Integer -> WeirdPeanoNumber
peanoFromSuccPred succ pred
  | succ == pred = Zero
  | succ > pred  = peanoSequence Succ (succ - pred - 1) $ Zero
  | succ < pred  = peanoSequence Pred (pred - succ - 1) $ Zero


simplify :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify wpn = let (sc, pd) = countSuccPred wpn in peanoFromSuccPred sc pd

equals :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
equals Zero        Zero        = True
equals (Succ fwpn) (Succ swpn) = equals fwpn swpn
equals (Pred fwpn) (Pred swpn) = equals fwpn swpn
equals _           _           = False

instance Eq WeirdPeanoNumber where
        (==) first second = equals (simplify first) (simplify second)

compareWPN :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
compareWPN first second | equals first second = EQ
                        | otherwise           = compareNeWPN first second

compareNeWPN :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
compareNeWPN (Succ fwpn) Zero        = GT
compareNeWPN (Pred fwpn) Zero        = LT
compareNeWPN Zero        (Succ swpn) = LT
compareNeWPN Zero        (Pred swpn) = GT

compareNeWPN (Succ fwpn) (Pred swpn) = GT
compareNeWPN (Pred fwpn) (Succ swpn) = LT

compareNeWPN (Succ fwpn) (Succ swpn) = compareNeWPN fwpn swpn
compareNeWPN (Pred fwpn) (Pred swpn) = compareNeWPN fwpn swpn

instance Ord WeirdPeanoNumber where
        compare first second = compareWPN (simplify first) (simplify second)

succMultiply :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
succMultiply Zero        b = Zero
succMultiply (Succ Zero) b = b
succMultiply (Succ a   ) b = b + succMultiply a b

multiply :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
multiply a b = case (signum a, signum b) of
  (0 , _ ) -> Zero
  (_ , 0 ) -> Zero
  (1 , 1 ) -> a `succMultiply` b
  (1 , -1) -> -(a `multiply` (-b))
  (-1, 1 ) -> -((-a) `multiply` b)
  (-1, -1) -> (-a) `multiply` (-b)

instance Num WeirdPeanoNumber where
  (+) Zero     b = b
  (+) (Succ a) b = Succ (a + b)
  (+) (Pred a) b = Pred (a + b)

  negate Zero       = Zero
  negate (Succ wpn) = Pred $ negate wpn
  negate (Pred wpn) = Succ $ negate wpn

  (*) a b = multiply (simplify a) (simplify b)
  
  signum wpn = case (simplify wpn) of
    Zero     -> 0
    (Succ _) -> 1
    (Pred _) -> -1

  abs wpn = case (signum wpn) of
    0  -> Zero
    1  -> wpn
    -1 -> -wpn

  fromInteger i | i == 0 = Zero
                | i < 0  = peanoFromSuccPred 0 (-i)
                | i > 0  = peanoFromSuccPred i 0

