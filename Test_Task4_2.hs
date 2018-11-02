module Test_Task4_2 where
import  Task4_2

x = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }
test = x ==  FourOf 5 8 10 12