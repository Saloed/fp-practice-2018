module Test_Task4_1 where
import  Task4_1

a = FunMonad(\s -> 3)
func = fmap id a
test_func = (fun a $ "") == (fun func $ "")
