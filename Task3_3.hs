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

instance Contravariant PSet where
  contramap f (PSet p) = PSet (p . f)

-- Так как для одного типа несколько определений завести нельзя
-- ниже объявлены несколько типов PSet
-- Функтор для всех видов PSer будет выглядеть также как выше,
-- так что реализованы только моноиды

newtype UnionPSet a = UnionPSet{ uContains :: (a -> Bool) }
newtype IntersectPSet a = IntersectPSet{ iContains :: (a -> Bool) }
newtype OuterPSet a = OuterPSet{ oContains :: (a -> Bool) }

instance Semigroup (UnionPSet a) where
        (<>) (UnionPSet left) (UnionPSet right) = UnionPSet (\x -> (left x) || (right x))

instance Monoid (UnionPSet a) where
        mempty = UnionPSet (\_ -> False)

instance Semigroup (IntersectPSet a) where
        (<>) (IntersectPSet left) (IntersectPSet right) = IntersectPSet (\x -> (left x) && (right x))

instance Monoid (IntersectPSet a) where
        mempty = IntersectPSet (\_ -> False)

instance Semigroup (OuterPSet a) where
        (<>) (OuterPSet left) (OuterPSet right) = OuterPSet (\x -> (not $ left x) && (not $ right x))

instance Monoid (OuterPSet a) where
        mempty = OuterPSet (\_ -> True)
