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

sinPrecisions :: Double -> Stream Double
sinPrecisions = todo

ePrecisions :: Stream Rational
ePrecisions = todo
