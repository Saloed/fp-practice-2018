module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)
-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Node {left::TreeMap v, right::TreeMap v, key::Integer, value::v}
                | Empty
                deriving (Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty _ = False
contains (Node left right nodeKey _) k | k == nodeKey = True
                                       | k < nodeKey  = contains left k
                                       | k > nodeKey  = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Empty = error "Key is not present"
lookup k (Node left right nodeKey nodeValue) | k == nodeKey = nodeValue
                                             | k < nodeKey  = lookup k left
                                             | k > nodeKey  = lookup k right

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Node Empty Empty k v
insert (k, v) (Node left right nodeKey nodeValue)
  | k == nodeKey = Node left right k v
  | k < nodeKey  = Node (insert (k, v) left) right nodeKey nodeValue
  | k > nodeKey  = Node left (insert (k, v) right) nodeKey nodeValue

minValueNode :: TreeMap v -> (Integer, v)
minValueNode (Node Empty _ key value) = (key, value)
minValueNode (Node left  _ _   _    ) = minValueNode left

fixNode :: TreeMap v -> TreeMap v -> TreeMap v
fixNode left right =
  let (k, v) = minValueNode right in Node left (remove k right) k v

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = Empty
remove i (Node left right nodeKey nodeValue)
  | i < nodeKey = Node (remove i left) right nodeKey nodeValue
  | i > nodeKey = Node left (remove i right) nodeKey nodeValue
  | i == nodeKey = case (left, right) of
    (Empty, Empty) -> Empty
    (left , Empty) -> left
    (Empty, right) -> right
    (left , right) -> fixNode left right

checkNodes :: TreeMap v -> (Integer, v) -> Integer -> (Integer, v)
checkNodes Empty current _ = current
checkNodes right@(Node _ _ rKey _) current i | rKey > i  = current
                                             | otherwise = nearestLE i right

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ Empty = error "nle not found"
nearestLE i (Node left right nodeKey nodeValue)
  | nodeKey == i = (nodeKey, nodeValue)
  | nodeKey > i  = nearestLE i left
  | nodeKey < i  = checkNodes right (nodeKey, nodeValue) i

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty = []
listFromTree (Node left right nodeKey nodeValue) =
  concat [listFromTree $ left, [(nodeKey, nodeValue)], listFromTree $ right]


-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = (listFromTree t) !! (fromInteger i)
