module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

generateWithIndex x i f =
  let value = f x i in Cons value $ generateWithIndex value (i + 1) f


instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

sinFun x n = let p = 2 * n + 1 in ((-1) ** n) * (x ** p) / (product [1 .. p])

halfPi = pi / 2

eFun n = 1 / (product [1 .. n])

moduloHelper :: Double -> Double -> Double
moduloHelper y m | y > 0 && m >= y  = 0
                 | y <= 0 && m <= y = 0
                 | y > 0 && m < 0   = if (y + m == y) then 0 else y + m
                 | y <= 0 && m > 0  = if (y + m == y) then 0 else y + m
                 | otherwise        = m

modulo :: Double -> Double -> Double
modulo x 0 = x
modulo x y =
  let m = x - y * (fromIntegral . floor) (x / y) in moduloHelper y m

sinBoundary :: Double -> Double
sinBoundary x = (modulo (x + halfPi) (2 * pi)) - halfPi

series f x i = x + (f i)

sinPrecisions :: Double -> Stream Double
sinPrecisions x =
  let f = sinFun $ sinBoundary $ x in generateWithIndex 0 0 (series f)

ePrecisions :: Stream Rational
ePrecisions = generateWithIndex 0 0 (series eFun)
