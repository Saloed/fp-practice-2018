module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf a1 b1 c1 d1) (FourOf a2 b2 c2 d2) =
        FourOf (a1 a2) (b1 b2) (c1 c2) (d1 d2)


instance Monad FourOf where
    return a = FourOf a a a a
    (>>=) (FourOf a b c d) f = FourOf aa bb cc dd
        where
        (FourOf aa _  _  _ ) = f a
        (FourOf _  bb _  _ ) = f b
        (FourOf _  _  cc _ ) = f c
        (FourOf _  _  _  dd) = f d
