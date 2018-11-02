module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap f (FunMonad fun) = FunMonad(f.fun)

instance Applicative FunMonad where
    pure a = FunMonad (\_-> a) 
    (<*>) (FunMonad f) (FunMonad fun) = FunMonad(\x -> f x $ fun x)


instance Monad FunMonad where
    return x = FunMonad(\s -> x)
    (>>=) (FunMonad fa) f = FunMonad(\s -> (fun.f.fa $ s) $ s)
