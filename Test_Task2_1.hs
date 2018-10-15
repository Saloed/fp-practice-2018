module Test_Task2_1 where

import Task2_1

import Prelude hiding (lookup)


root0 = emptyTree
root1 = insert (30, 1) root0
root2 = insert (20, 2) root1
root3 = insert (40, 3) root2
root4 = insert (10, 4) root3
root5 = insert (5, 5) root4

tree = root5

test_contains = [(contains tree 30) == True, (contains tree 6) == False]
look_ok = lookup 20 tree 
look_err = lookup 6 tree 

test_delete = remove 20 tree

lst = listFromTree tree
new_tree = treeFromList lst
list_eq = lst == listFromTree new_tree

ks = kMean 0 tree
