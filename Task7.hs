module Task7 where

import Todo(todo)

data Deque a = ChangeMe

-- Пустая очередь
empty :: Deque a
empty = todo

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront = todo

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront = todo

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack = todo

-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack = todo
