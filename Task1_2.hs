module Task1_2 where

import Todo(todo)

import Prelude hiding (sin, cos, gcd)


series f = [f(n) | n <- [0..]]

sinFun x n =
  let p = 2 * n + 1 in
  ((-1) ** n) * (x ** p) / (product [1..p])

cosFun x n =
  let p = 2 * n in
  ((-1) ** n) * (x ** p) / (product [1..p])

halfPi = pi / 2

moduloHelper :: Double -> Double -> Double
moduloHelper y m
    | y > 0 && m >= y = 0
    | y <= 0 && m <= y = 0
    | y > 0 && m < 0 = if (y + m == y) then 0 else y + m
    | y <= 0 && m > 0 = if (y + m == y) then 0 else y + m
    | otherwise = m

modulo :: Double -> Double -> Double
modulo x 0 = x
modulo x y = 
  let m = x - y * (fromIntegral.floor) (x / y) in
  moduloHelper y m

sinBoundary :: Double -> Double
sinBoundary x = (modulo (x + halfPi) (2*pi)) - halfPi

cosBoundary :: Double -> Double
cosBoundary x = modulo x (2 * pi)

computeWithBoundArgument x fun boundary = 
    let bounded = boundary x in 
    sum (take 100 (series (fun bounded)))

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = computeWithBoundArgument x sinFun sinBoundary

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x =  computeWithBoundArgument x cosFun cosBoundary

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y
  | x == y = x
  | x > y = gcd (x - y)  y
  | x < y = gcd x (y - x)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = ((floor (sqrt (fromIntegral (to - 1)))) - (ceiling (sqrt (fromIntegral from)))) >= 0

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

square x = x * x

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y
  | y == 0 = 1
  | even y = pow (square x) (y `div` 2)
  | otherwise = x * (pow (square x) ((y - 1) `div` 2))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x
  | x == 1 = False
  | otherwise = not (any (\i -> (x `mod` i) == 0) [2..floor(sqrt(fromIntegral x))])

type Point2D = (Double, Double)

multiply :: (Double, Double) -> Double
multiply pair =
 let (x, y) = pair in
 x * y

summation :: [Double] -> [Double] -> Double
summation x y = sum(map multiply (zip x y))

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
    let
      (firstX, firstY) = head(points)
      (nX, nY) = last(points)
      x = map fst points
      y = map snd points
      firstSum = summation x (tail y)
      secondSum = summation (tail x) y
      resultSum = firstSum + nX * firstY - secondSum - firstX * nY
    in
    (abs resultSum) / 2

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
