module Test_Task7 where
import Task7


q1 = foldl pushFront empty [1..10]
q2 = foldl pushBack q1 [11..20]



pop_all (Queue _ [] _ []) _ = []
pop_all q popper = a : pop_all nq popper
    where (a, nq) = popper q

test_pop_front = pop_all q2 popFront
test_pop_back = pop_all q2 popBack


