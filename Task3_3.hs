module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


-- Тип PSet является контравариантным для a. 
-- Для него нельзя определить Functor, т.к. он ковариантный
-- Можно определить только контрвариантый функтор
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
-- Определения взято из пакета Data.Functor.Contravariant

-- Так как для одного типа несколько определений завести нельзя
-- ниже объявлены несколько типов PSet

newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet a = PSet{ contains :: (a -> Bool) }
