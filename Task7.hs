module Task7 where

import Todo(todo)

data Deque a = Queue Int [a] Int [a]
                deriving (Show)

-- Пустая очередь
empty :: Deque a
empty = Queue 0 [] 0 []

-- Banker's Dequeue (thx Chris Okasaki)
balanceConst :: Int
balanceConst = 4

queueSizes :: Deque a -> (Int, Int)
queueSizes (Queue insize _ outsize _) = (size, restSize)
 where
  sizeSum  = insize + outsize
  size     = sizeSum `div` 2
  restSize = sizeSum - size

balanceInp :: Deque a -> Deque a
balanceInp queue@(Queue insize inp outsize out) = newQueue
 where
  (size  , restSize) = queueSizes queue
  (newInp, restInp ) = splitAt size inp
  newOut             = out ++ reverse restInp
  newQueue           = Queue size newInp restSize newOut

balanceOut :: Deque a -> Deque a
balanceOut queue@(Queue insize inp outsize out) = newQueue
 where
  (size  , restSize) = queueSizes queue
  (newOut, restOut ) = splitAt size out
  newInp             = inp ++ reverse restOut
  newQueue           = Queue restSize newInp size newOut

balance :: Deque a -> Deque a
balance queue@(Queue insize inp outsize out)
  | insize > balanceConst * outsize + 1 = balanceInp queue
  | outsize > balanceConst * insize + 1 = balanceOut queue
  | otherwise                           = queue

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Queue insize inp outsize out) x =
  balance $ Queue (insize + 1) (x : inp) outsize out

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront (Queue _ [] _ []  ) = error "Queue is empty"
popFront (Queue _ [] _ [el]) = (el, empty)
popFront (Queue insize (ih : it) outsize out) =
  (ih, balance $ Queue (insize - 1) it outsize out)

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack (Queue insize inp outsize out) x =
  balance $ Queue insize inp (outsize + 1) (x : out)


-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack (Queue _ []   _ []) = error "Queue is empty"
popBack (Queue _ [el] _ []) = (el, empty)
popBack (Queue insize inp outsize (oh : ot)) =
  (oh, balance $ Queue insize inp (outsize - 1) ot)


{-
Метод банкира

Пусть все операции приносят 2*c + 1 денег, то есть 8
Перекидывание стоит N денег, где N это общее количество элементов
в очереди.
Сразу после перекидывания или в начальный момент
размеры обоих списков равны min (insize, outsize) = max(insize, outsize) = N/2 = N_b
Ближаёшее перекидывание произойдёт когда 
max(insize, outsize) > c * min (insize, outsize)
Есть 2 способа достичь перекидывания из точки равновесия 
наименьшим числом операций:
удалять из наименьшего списка или добавлять в наибольший. 
При удалении необходимо совершить
(c - 1)/c * N_b операций, что принесёт
2 * (с - 1) * N_b денег, а стоимость
перекладывания будет 1.25 * N_b.
Так как 2 * (c - 1) > 1.25, то баланс останется положительным.
При добавлении в наибольший список, необходимо совершить (c - 1) * N_b
добавлений, что принесёт 2 * c * (c - 1) * N_b денег.
Стоимость перекладывания при этом будет 
(c + 1) * N_b. Так как 2 * c * (c - 1) > (c + 1), то
баланс останется положительным. 
Любая другая последовательность операций принесёт больше денег, а следовательно
баланс останется положительным.

Метод физика

Пусть
Ф = Х * max(insize, outsize)
где Х -- константа
Ф(0) = 0 

При добавлениии в начало или конец потенциал увеличивается на X
При удалении из начала или конца потенциал уменьшается на X
Перекидывание уменьшает потенциал следующим образом:
  Так как перекидывание возникает только в случае, когда
  размер больше списка больше меньшего в c раз, то 
  размер меньшего составляет N / (c + 1), а размер большего
  составляет c * N / (c + 1).
  Тогда размер перекидываемого кусочка равен
  dN = (c / (c + 1) - (c + 1) / 2) * N    
  При X = (c / (c + 1) - (c + 1) / 2)
  получается, что разница потенциалов N

  Так как сложность перикидывания N, то
  амортизированная сложность операции О(1).

-}