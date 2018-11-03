module Test_Task6 where
import Task6

list = [10, 5, 7, 8, 6, 3, 4, 2, 1, 15, 16, 18, 17, 19, 12, 13, 14, 11]
tree = foldl insert Empty list
test_find = all (find tree) list
test_findN = all (not . (find tree)) [0]

validate Empty                     _ = True
validate (Node Empty left right v) _ = (validate left v) && (validate right v)
validate (Node (Node _ _ _ pv) left right v) parentV =
  pv == parentV && (validate left v) && (validate right v)

test_parent = validate tree 0

trees_without_elem = map (remove tree) list

check_tree (tree, elem) =
  let inels = filter (/= elem) list
      f     = find tree
  in  (validate tree 0) && (all f inels) && (not $ f elem)

test_remove = map check_tree (zip trees_without_elem list)
