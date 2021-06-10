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
-- Join + SequenceA

-- (>>=) m f = join $ fmap f m
-- traverse f m = sequenceA $ fmap f m




infixl 2 $>>=
infixl 2 =<<$
($>>=) :: (Monad m, Traversable t) => t (m a) -> (a -> m b) -> m (t b)
($>>=) m f = traverse (>>= f) m
(=<<$) f m = m $>>= f

infixl 2 >$>=
infixl 2 =<$<
(>$>=) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(>$>=) m f = m >>= traverse f
(=<$<) f m = m >$>= f

infixl 2 >>$=
infixl 2 =$<<
(>>$=) :: (Monad m, Traversable t) => m a -> (a -> t (m b)) -> m (t b)
(>>$=) m f = m >>= sequenceA . f
(=$<<) f m = m >>$= f

{-
(>>=$) :: (Monad m, Traversable t) => m a -> (a -> m (t b)) -> m (t b)
this is the same as (>>=)
-}








infix 2 *>>=
infix 2 =<<*
(*>>=) :: (Functor f, Monad t, Traversable t) => f (t a) -> (a -> t b) -> f (t b)
(*>>=) m f = fmap (join . traverse f) m
(=<<*) f m = m *>>= f

infix 2 >*>=
infix 2 =<*<
(>*>=) :: (Applicative f, Monad t, Traversable t) => t (f a) -> (a -> t b) -> f (t b)
(>*>=) m f = fmap (>>= f) $ sequenceA m
(=<*<) f m = m >*>= f

infix 2 >>*=
infix 2 =*<<
(>>*=) :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
(>>*=) m f = fmap join $ traverse f m
(=*<<) f m = m >>*= f

infix 2 >>=*
infix 2 *=<<
(>>=*) :: (Applicative f, Monad t, Traversable t) => t a -> (a -> t (f b)) -> f (t b)
(>>=*) m f = sequenceA $ m >>= f
(*=<<) f m = m >>=* f








infixl 2 >>==
infixl 2 ==<<
(>>==) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
(>>==) m f = m >>= \t -> t >>*= f
(==<<) f m = m >>== f

--infixl 2
--infixl 2
--() :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
--() t f = t >>*= \m -> m >>= f
--() f m = m _ f

--infixl 2
--infixl 2
--() :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
--() m f = m >>= \t -> t >>=* f
--() f m = m _ f

--infixl 2
--infixl 2
--() :: (Monad t1, Monad t2, Traversable t1, Traversable t2) => t1 (t2 a) -> (a -> t1 (t2 b)) -> t2 (t1 b)
--() m f = m >>=* \t -> t >>*= f
--() f m = m _ f
