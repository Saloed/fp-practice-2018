module Test_Task5_1 where
import Task5_1

x = list2dlist [0 .. 10]

test_index = map (index x) [0 .. 10] == [0 .. 10]


goRight DNil                 = []
goRight (DCons left v DNil ) = (goLeft left) ++ [v]
goRight (DCons _    _ right) = goRight right
goLeft (DCons DNil v _) = [v]
goLeft (DCons left v _) = (goLeft left) ++ [v]

collectL = goRight

y0 = DNil
y1 = insertAt y0 0 0
y2 = insertAt y1 1 1
y3 = insertAt y2 2 2
y4 = insertAt y3 3 3
y5 = insertAt y4 4 4
y = y5

removed = removeAt y 2
