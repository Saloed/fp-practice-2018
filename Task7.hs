module Task7 where

import Todo(todo)

data Deque a = Queue Int [a] Int [a]

-- Пустая очередь
empty :: Deque a
empty = Queue 0 [] 0 []

-- Banker's Dequeues (thx Chris Okasaki)
balanceConst :: Int
balanceConst = 4

queueSizes :: Deque a -> (Int, Int)
queueSizes (Queue insize _ outsize _) = (size, restSize)
   where
    sizeSum = insize + outsize
    size = sizeSum `div` 2
    restSize = sizeSum - size

balanceInp :: Deque a -> Deque a
balanceInp queue @ (Queue insize inp outsize out) = Queue size newInp restSize newOut
  where
    (size, restSize) = queueSizes queue
    (newInp, restInp)  = splitAt size inp
    newOut = out ++ reverse restInp

balanceOut :: Deque a -> Deque a
balanceOut queue@(Queue insize inp outsize out) = Queue restSize newInp size newOut
  where
    (size, restSize) = queueSizes queue
    (newOut, restOut)  = splitAt size out
    newInp = inp ++ reverse restOut

balance :: Deque a -> Deque a
balance  queue@(Queue insize inp outsize out) | insize > balanceConst * outsize + 1 = balanceInp queue
                                              | outsize > balanceConst * insize + 1 = balanceOut queue
                                              | otherwise = queue

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Queue insize inp outsize out) x = balance $ Queue (insize + 1) (x: inp) outsize out

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront (Queue _ [] _ []) = error "Queue is empty"
popFront (Queue _ [] _ [el]) = (el, empty)
popFront (Queue insize (ih:it) outsize out) = (ih, balance $ Queue (insize - 1) it outsize out)

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack = todo

-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack = todo
