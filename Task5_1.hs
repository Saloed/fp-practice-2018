module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index DNil _ = error "Empty list"
index (DCons _ a right) i | i == 0    = a
                          | otherwise = index right (i - 1)

updateLeft DNil          _    = DNil
updateLeft (DCons _ v r) left = item
 where
  right = updateLeft r item
  item  = DCons left v right


insertAt' left DNil  0     value = DCons left value DNil
insertAt' left DNil  index value = error "List too small"
insertAt' left right 0     value = item
 where
  next = updateLeft right item
  item = DCons left value next
insertAt' left (DCons _ v r) index value = item
 where
  next = insertAt' item r (index - 1) value
  item = DCons left v next

removeAt' :: DList a -> DList a -> Int -> DList a
removeAt' left DNil              index = error "List too small"
removeAt' left (DCons _ _ right) 0     = updateLeft right left
removeAt' left (DCons _ v right) index = item
 where
  next = removeAt' item right (index - 1)
  item = DCons left v next

insertAt :: DList a -> Int -> a -> DList a
insertAt list index value = insertAt' DNil list index value

removeAt :: DList a -> Int -> DList a
removeAt list index = removeAt' DNil list index
