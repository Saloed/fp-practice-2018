module Task1_1 where

import Todo(todo)

data BinaryOperation = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, operation :: BinaryOperation } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
infixl 5 |+|

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
infixl 5 |-|

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Times
infixl 6 |*|


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
    let _replaceVar x = replaceVar varName replacement x in
        case expression of
            Variable var -> if(var == varName) then replacement else expression
            BinaryTerm lhs rhs operation -> BinaryTerm (_replaceVar lhs) (_replaceVar rhs) operation
            _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression =
    case expression of
        BinaryTerm lhs rhs operation-> simplify lhs rhs operation
        _ -> expression

simplify :: Term -> Term -> BinaryOperation -> Term
simplify lhs rhs operation=
    let left = evaluate(lhs)
        right = evaluate(rhs)
    in case (left, right, operation) of
        (IntConstant l, IntConstant r, Plus) -> IntConstant (l + r)
        (IntConstant l, IntConstant r, Minus) -> IntConstant (l - r)
        (IntConstant l, IntConstant r, Times) -> IntConstant (l * r)
        (IntConstant 0, r , Plus) -> r
        (IntConstant 0, r , Times) -> IntConstant 0
        (IntConstant 1, r , Times) -> r
        (l, IntConstant 0 , Plus) -> l
        (l, IntConstant 0 , Minus) -> l
        (l, IntConstant 0 , Times) -> IntConstant 0
        (l, IntConstant 1 , Times) -> l
        _ -> BinaryTerm left right operation
