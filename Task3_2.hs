module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList' ::  [a]  -> ReverseList a -> [a]
rlistToList' res RNil = res
rlistToList' res  (RCons prev it) = rlistToList' (it:res) prev 

rlistToList :: ReverseList a -> [a]
rlistToList = rlistToList' []


listToRList :: [a] -> ReverseList a
listToRList = foldl RCons RNil

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
        show RNil = "[]"
        show (RCons prev it) = (show prev) ++ ":" ++ (show it)

instance (Eq a) => Eq (ReverseList a) where
        (==) RNil RNil = True
        (==) (RCons lprev lit) (RCons rprev rit) | lit /= rit = False
                                                 | otherwise  = lprev == rprev
        (==) _ _ = False

instance (Ord a) => Ord (ReverseList a) where
        (<=) RNil _ = True
        (<=) _ RNil = False
        (<=) (RCons lprev lit) (RCons rprev rit) | lit == rit = lprev <= rprev
                                                 | lit < rit = True
                                                 | lit > rit = False

instance Semigroup (ReverseList a) where
        (<>) first RNil = first
        (<>) first (RCons prev el) = RCons (first <> prev) el

instance Monoid (ReverseList a) where
        mempty = RNil

instance Functor ReverseList where
        fmap f RNil = RNil
        fmap f (RCons prev el) = RCons (fmap f prev) (f el)
