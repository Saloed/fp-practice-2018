module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList = todo

listToRList :: [a] -> ReverseList a
listToRList = todo

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
