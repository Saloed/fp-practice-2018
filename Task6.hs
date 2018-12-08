module Task6 where

import Todo(todo)

data LinkedTree a = Node (LinkedTree a) (LinkedTree a) (LinkedTree a) a | Empty


instance (Eq a) => Eq (LinkedTree a) where
  (==) Empty Empty = True
  (==) (Node _ ll lr lv) (Node _ rl rr rv) = lv == rv && ll == rl && lr == rr
  (==) _ _ = False

-- declare BinTree a to be an instance of Show
instance (Show a) => Show (LinkedTree a) where
 show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
  where

   treeshow pref Empty                  = ""
   treeshow pref (Node _ Empty Empty x) = (pshow pref x)

   treeshow pref (Node _ left Empty x) =
    (pshow pref x) ++ "\n" ++ (showSon pref "`--" "   " left)

   treeshow pref (Node _ Empty right x) =
    (pshow pref x) ++ "\n" ++ (showSon pref "`--" "   " right)

   treeshow pref (Node _ left right x) =
    (pshow pref x)
      ++ "\n"
      ++ (showSon pref "|--" "|  " right)
      ++ "\n"
      ++ (showSon pref "`--" "   " left)

   showSon pref before next t = pref ++ before ++ treeshow (pref ++ next) t

   pshow pref x = replace '\n' ("\n" ++ pref) (show x)

   replace c new string = concatMap (change c new) string
    where
    change c new x | x == c    = new
                   | otherwise = x : []


replaceParent Empty                 _      = Empty
replaceParent (Node _ left right v) parent = node
 where
  l    = replaceParent left node
  r    = replaceParent right node
  node = Node parent l r v

insertRight parent (Node _ left right v) value = node
 where
  l    = replaceParent left node
  r    = insert' node right value
  node = Node parent l r v

insertLeft parent (Node _ left right v) value = node
 where
  l    = insert' node left value
  r    = replaceParent right node
  node = Node parent l r v

insert' :: (Ord a) => LinkedTree a -> LinkedTree a -> a -> LinkedTree a
insert' parent Empty value = Node parent Empty Empty value
insert' parent tree@(Node _ _ _ v) value
  | value == v = tree
  | value > v  = insertRight parent tree value
  | value < v  = insertLeft parent tree value

minValueNode :: (Ord a) =>  LinkedTree a -> a
minValueNode (Node _ Empty _ value) = value
minValueNode (Node _ left  _ _    ) = minValueNode left

removeLeft parent (Node _ left right v) elem = node
 where
  l    = remove' node left elem
  r    = replaceParent right node
  node = Node parent l r v

removeRight parent (Node _ left right v) elem = node
 where
  l    = replaceParent left node
  r    = remove' node right elem
  node = Node parent l r v

fixNode
  :: (Ord a) => LinkedTree a -> LinkedTree a -> LinkedTree a -> LinkedTree a
fixNode parent left right = node
 where
  elem = minValueNode right
  l    = replaceParent left node
  r    = remove' node right elem
  node = Node parent l r elem

remove' :: (Ord a) => LinkedTree a -> LinkedTree a -> a -> LinkedTree a
remove' _ Empty _ = Empty
remove' parent tree@(Node _ left right v) elem
  | elem > v = removeRight parent tree elem
  | elem < v = removeLeft parent tree elem
  | elem == v = case (left, right) of
    (Empty, Empty) -> Empty
    (left , Empty) -> replaceParent left parent
    (Empty, right) -> replaceParent right parent
    (left , right) -> fixNode parent left right

find :: (Ord a) => LinkedTree a -> a -> Bool
find Empty _ = False
find (Node _ left right v) elem | elem == v = True
                                | elem > v  = find right elem
                                | elem < v  = find left elem


updateParentLeft :: (Eq a) => LinkedTree a -> LinkedTree a -> LinkedTree a -> LinkedTree a
updateParentLeft Empty _    _   = Empty
updateParentLeft currP@(Node parent left right value) prev new = newParent
 where
  newNodeParent = updateParent parent currP newParent
  newRight      = replaceParent right newParent
  newParent     = Node newNodeParent new newRight value

updateParentRight :: (Eq a) => LinkedTree a -> LinkedTree a -> LinkedTree a -> LinkedTree a
updateParentRight Empty _    _   = Empty
updateParentRight currP@(Node parent left right value) prev new = newParent
 where
  newNodeParent = updateParent parent currP newParent
  newLeft       = replaceParent left newParent
  newParent     = Node newNodeParent newLeft new value

updateParent :: (Eq a) =>  LinkedTree a -> LinkedTree a -> LinkedTree a -> LinkedTree a
updateParent Empty _ _ = Empty
updateParent currP@(Node _ left right _) prev new
  | prev == left  = updateParentLeft currP prev new
  | prev == right = updateParentRight currP prev new
  | otherwise     = error "WTF???"


insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert Empty                    value = insert' Empty Empty value
insert tree@(Node Empty  _ _ _) value = insert' Empty tree value
insert tree@(Node parent _ _ _) value = node
 where
  node      = insert' newParent tree value
  newParent = updateParent parent tree node

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove Empty                    _    = Empty
remove tree@(Node Empty  _ _ _) elem = remove' Empty tree elem
remove tree@(Node parent _ _ _) elem = node
 where
  node      = remove' newParent tree elem
  newParent = updateParent parent tree node
