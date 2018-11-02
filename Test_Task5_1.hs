module Test_Task5_1 where
import  Task5_1

x = list2dlist [0..10]
test_index = map (index x) [0..10] == [0..10]
