module Test_Task2_2 where

import Task2_2
import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

a = [1, 2, 3, 4, 5]
c = [7, 3, 2]

lsum_a = foldl (+) 0 a
rsum_a = foldr (+) 0 a

square x = x * x
msquare_a = map square a

prod_a = product a

test_unfold = take 5 $ unfoldr (\n -> Just (show n, n + 1)) 0

test_filter = filter (/= 2) a
test_nfilter = filterNot (== 2) a

b = [Just 1, Just 4, Nothing, Just 5, Nothing]
test_catMaybes = catMaybes b

mat = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
test_mat = diagonal mat

test_present = elem 2 a
test_not_present = not (elem 6 a)

test_range = rangeTo 3 15 2

test_append = append c a

lst = [1..10]
test_group_1 = groups lst 3
test_group_2 = groups lst 1
test_group_3 = groups lst 5
