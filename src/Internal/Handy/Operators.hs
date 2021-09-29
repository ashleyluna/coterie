{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Internal.Handy.Operators where

import ClassyPrelude.Yesod hiding ((<&&>))




-- "faster" version of $
infixr 8 ¢
(¢) :: (t1 -> t2) -> t1 -> t2
(¢) f = f

infixl 4 <$$>
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = fmap $ fmap f --fmap fmap fmap

infixl 1 <&&>
(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) a f = f <$$> a

--(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
--(<$$$>) = fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap fmap

--infixl 1 ??
--(??) :: Functor f => f (a -> b) -> a -> f b
--f ?? a = fmap ($ a) f
