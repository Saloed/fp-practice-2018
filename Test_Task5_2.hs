module Test_Task5_2 where
import Task5_2

takeFromStream 0 (Cons head tail) = head
takeFromStream i (Cons head tail) = takeFromStream (i - 1) tail

a = sinPrecisions 100
e = ePrecisions
test_sin = takeFromStream 20 a
test_e = takeFromStream 20 e

