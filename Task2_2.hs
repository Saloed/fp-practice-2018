module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z list = case list of
  []      -> z
  (h : t) -> foldl f (f z h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z list = case list of
  []      -> z
  (h : t) -> f h (foldr f z t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f z =
  let value = f z
  in  case value of
        Just (x, next) -> (x : (unfoldr f next))
        Nothing        -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

mapElement :: (a -> b) -> a -> [b] -> [b]
mapElement f elem list = ((f elem) : list)

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f list = foldr (mapElement f) [] list

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product [] = 0
product list = foldl (*) 1 list

filterElem :: (a -> Bool) -> a -> [a] -> [a]
filterElem pred elem res = if pred elem then (elem : res) else res

filter :: (a -> Bool) -> [a] -> [a]
filter pred list = let filterFn = (filterElem pred) in foldr filterFn [] list

isJust (Just _) = True
isJust _        = False
getJustValue (Just v) = v

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes list = (map getJustValue) $ (filter isJust) list

nextDiagElem :: Int -> (Int, [a]) -> [a] -> (Int, [a])
nextDiagElem rowSize (idx, res) row
  | idx < rowSize = (idx + 1, (row !! idx : res))
  | otherwise     = (idx, res)

elemCounter :: a -> Int -> Int
elemCounter _ count = count + 1

size :: [a] -> Int
size = foldr elemCounter 0

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal mat =
  let rowSize        = size $ head mat
      diagElemGetter = nextDiagElem rowSize
  in  reverse $ snd $ foldl diagElemGetter (0, []) mat

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f list = let nf = not . f in filter nf list

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x list = (filter ( == x) list) /= []

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step =
  unfoldr (\n -> if n < to then Just (n, n + step) else Nothing) from

appendHelper :: a -> [a] -> [a]
appendHelper elem res = (elem : res)

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append []    []     = []
append first []     = first
append []    second = second
append first second = foldr appendHelper second first


grouper :: Integer -> ([[a]], Integer) -> a -> ([[a]], Integer)
grouper _ ([], 0) elem = ([[elem]], 1)
grouper chunkSize ((h : t), size) elem | size == chunkSize = ([elem] : h : t, 1)
                                       | otherwise = ((elem : h) : t, size + 1)

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n =
  let groupFn = (grouper n)
  in  reverse . (map reverse) . fst $ foldl groupFn ([], 0) lst
