module Test_Task3_2 where

import           Task3_2

rlst = listToRList [0 .. 10]
lst = rlistToList rlst
test_eq = rlst == rlst
test_neq = not $ rlst == listToRList [0 .. 9]

_to a b = (listToRList a) <= (listToRList b)
_nto a b = not (_to a b)

test_order =
  [ _to "" ""
  , _to "" "a"
  , _nto "a" ""
  , _to "a"  "b"
  , _to "a"  "bc"
  , _to "aa" "b"
  , _nto "bb" "b"
  , _nto "bb" "ba"
  , _to "bb" "bb"
  ]
f = listToRList "ab"
s = listToRList "cd"
test_monad = rlistToList (f <> s) == "abcd"

test_functor = (rlistToList $ (+1) <$> rlst) == [1..11]