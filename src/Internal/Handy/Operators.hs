{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.Handy.Operators where

import ClassyPrelude.Yesod

import Control.Monad




-- "faster" version of $
infixr 8 ¢
(¢) f a = f a


--infixr 7 <.>
--(<.>) :: Functor f => (b -> c) -> f (a -> b) -> f (a -> c)
--(<$.>) f = fmap ((.) f)
--
--infixr 7 <<.>>
--(<<.>>) :: Functor f => (b -> c) -> f (g (a -> b)) -> f (g (a -> c))
--(<<.>>) = fmap . (<.>)





--------------------------------------------------------------------------------
-- Join + Sequence

-- (>>=) m f = join $ fmap f m
-- traverse f m = sequenceA $ fmap f m




-- ($>>=) m f = sequenceA $ fmap (join . fmap f) m :: t (m a) -> (a -> m b) -> m (t b)
-- (>$>=) m f = join $ fmap (sequenceA . fmap f) m :: m (t a) -> (a -> m b) -> m (t b)

-- ______ m f = join $ fmap (sequenceA . f) m      :: m a -> (a -> t (m b)) -> m (t b)
-- (>>=)  m f = join $ fmap f m                    :: m a -> (a -> m (t b)) -> m (t b)
              -- ^ literally just bind

-- (>>==) m f = m >>= \t -> t >>*= f               :: m (t a) -> (a -> m (t b)) -> m (t b)




-- ______ m f = fmap (join . sequenceA . fmap f) m :: t (m a1) -> (a1 -> m a2) -> t (m a2)
-- ______ m f = fmap (join . fmap f) $ sequenceA m :: m (t a1) -> (a1 -> m a2) -> t (m a2)

-- (>>*=) m f = fmap join $ sequenseA $ fmap f m   :: m a -> (a -> t (m b)) -> t (m b)
-- ______ m f = sequenceA $ join $ fmap f m        :: m a -> (a -> m (t b)) -> t (m b)

-- :: m (t a) -> (a -> m (t b)) -> t (m b)




infixl 2 $>>=
infixl 2 =<<$
($>>=) :: (Monad m, Traversable t) => t (m a) -> (a -> m b) -> m (t b)
($>>=) m f = traverse (>>= f) m
(=<<$) f m = m $>>= f

infixl 2 =<$<
infixl 2 >$>=
(>$>=) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(>$>=) m f = m >>= traverse f
(=<$<) f m = m >$>= f




infix 2 >>*=
(>>*=) :: (Monad t, Traversable t, Applicative f) => t a -> (a -> f (t b)) -> f (t b)
(>>*=) m f = fmap join $ traverse f m
