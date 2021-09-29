{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Control.Monad.Traversable
 ((>>-=)
 ,(>>=-)
 ,(->==)
 ,(>>==)
 --,(>-=#)
 --,(>>-#)
 --,(>>#-)
 --,(->=#)
 --,(>>=#)
 --,(>>#=)
 --,(>>##)
 ,Monad(..)
 ) where

import Control.Monad
import ClassyPrelude.Yesod

import Data.Functor.Compose

{-

Most useful operators

(>>=-) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
       Useful for
         - Doing IO while ignoring the inner container
         - Skipping an unnecessary case analysis
(->==) :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
       Useful for
         - Not having to make a line that does nothing "Nothing -> return Nothing"
         - Skipping an unnecessary case analysis
(>>==) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
       Useful for
         - Not having to use a Monad Transformer Stack
(>>-=) :: (Functor m, Monad t) => m (t a) -> (a -> t b) -> m (t b)
       Useful for
         - Doing Maybe Effects under IO
         - Skipping unnecessary do notation

-}


--------------------------------------------------------------------------------
-- Group 1
-- Outer layer is Monadic


{-
(>--=) :: (Functor m) => m a -> (a -> t b) -> m (t b)
this is the same as (<&>)

(->=-) :: (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
this is the same as `for`

(>-==) :: (Monad m, Traversable t) => m a -> (a -> m (t b)) -> m (t b)
this is the same as (>>=)
-}

infixl 2 >>-= 
infixr 2 =-<<
(>>-=) :: (Functor m, Monad t) => m (t a) -> (a -> t b) -> m (t b)
(>>-=) = bindU
(=-<<) f m = m >>-= f

infixl 2 >>=-
infixr 2 -=<<
(>>=-) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(>>=-) = bindT
(-=<<) f m = m >>=- f

infixl 2 ->==
infixr 2 ==<-
(->==) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> m (t b)) -> m (t b)
(->==) = bindUT
(==<-) f m = m ->== f

infixl 2 >>==
infixr 2 ==<<
(>>==) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
(>>==) = bind2
(==<<) f m = m >>== f




--------------------------------------------------------------------------------
-- Group 2, Variations on Group 1
-- Pre-Sequence Function and/or Value


infixl 2 >-=#
infixr 2 #=-<
(>-=#) :: (Monad m, Traversable t) => m a -> (a -> t (m b)) -> m (t b)
(>-=#) = bindF
(#=-<) f m = m >-=# f

infixl 2 >>-# -- # overlaps with -, # should be on left
infixr 2 #-<<
(>>-#) :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a -> t b) -> m (t b)
(>>-#) = bindUV
(#-<<) f m = m >>-# f

infixl 2 >>#-
infixr 2 -#<<
(>>#-) :: (Monad m, Traversable t) => t (m a) -> (a -> m b) -> m (t b)
(>>#-) = bindTV
(-#<<) f m = m >>#- f

infixl 2 ->=#
infixr 2 #=<-
(->=#) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> t (m b)) -> m (t b)
(->=#) = bindUTF
(#=<-) f m = m ->=# f

infixl 2 >>=#
infixl 2 #=<<
(>>=#) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
(>>=#) = bind2F
(#=<<) f m = m >>=# f

infixl 2 >>#=
infixl 2 =#<<
(>>#=) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
(>>#=) = bind2V
(=#<<) f m = m >>#= f

infixl 2 >>##
infixl 2 ##<<
(>>##) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)
(>>##) = bind2FV
(##<<) f m = m >>## f




--------------------------------------------------------------------------------
-- The Compose Monad


instance (Monad m, Monad t, Traversable t) => Monad (Compose m t) where
  return a = Compose $ return $ return a
  m >>= f = Compose $ getCompose m >>== getCompose . f



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
(>>=) m f = join $ fmap f m
traverse f m = sequenceA $ fmap f m
-}


bind   ::  Monad m                                => m    a  -> (a -> m    b ) -> m    b
bindU  :: (Functor m,     Monad t)                => m (t a) -> (a ->    t b ) -> m (t b)
bindT  :: (Monad m,                Traversable t) => m (t a) -> (a -> m    b ) -> m (t b)
bindUT :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> m (t b)) -> m (t b)
bind2  :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)

bind   m f = join $      (                        fmap f) $ m -- >>= / >$>=
bindU  m f =        fmap (     join .             fmap f) $ m -- >>$=
bindT  m f = join $ fmap (            sequenceA . fmap f) $ m -- >>=$
bindUT m f =             (fmap join . sequenceA . fmap f) $ m -- $>>=
bind2  m f = join $ fmap (fmap join . sequenceA . fmap f) $ m -- >>==




bindF   :: (Monad m,                Traversable t) => m    a  -> (a -> t (m b)) -> m (t b)
bindUV  :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a ->    t b ) -> m (t b)
bindTV  :: (Monad m,                Traversable t) => t (m a) -> (a -> m    b ) -> m (t b)
bindUTF :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> t (m b)) -> m (t b)
bind2F  :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
bind2V  :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
bind2FV :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)

seqFuncA bind m f = bind m (sequenceA . f)
seqValA  bind m f = bind (sequenceA m) f

bindF   = seqFuncA           bind   -- >># / >$>#
bindUV  = seqValA            bindU  -- >>$#
bindTV  = seqValA            bindT  -- >>#$
bindUTF = seqFuncA           bindUT -- $>>#
bind2F  = seqFuncA           bind2  -- >>=#
bind2V  = seqValA            bind2  -- >>#=
bind2FV = seqValA $ seqFuncA bind2  -- >>##



{-
bind    m f = join $      (                        fmap              f )             $ m :: m    a  -> (a -> m    b ) -> m    b
bindF   m f = join $      (                        fmap (sequenceA . f))             $ m :: m    a  -> (a -> t (m b)) -> m (t b)
bindU   m f =        fmap (     join .             fmap              f )             $ m :: m (t a) -> (a ->    t b ) -> m (t b)
bindUV  m f =        fmap (     join .             fmap              f ) . sequenceA $ m :: t (m a) -> (a ->    t b ) -> m (t b)
bindT   m f = join $ fmap (            sequenceA . fmap              f )             $ m :: m (t a) -> (a -> m    b ) -> m (t b)
bindTV  m f = join $ fmap (            sequenceA . fmap              f ) . sequenceA $ m :: t (m a) -> (a -> m    b ) -> m (t b)
bindUT  m f =             (fmap join . sequenceA . fmap              f )             $ m ::    t a  -> (a -> m (t b)) -> m (t b)
bindUTF m f =             (fmap join . sequenceA . fmap (sequenceA . f))             $ m ::    t a  -> (a -> t (m b)) -> m (t b)
bind2   m f = join $ fmap (fmap join . sequenceA . fmap              f )             $ m :: m (t a) -> (a -> m (t b)) -> m (t b)
bind2F  m f = join $ fmap (fmap join . sequenceA . fmap (sequenceA . f))             $ m :: m (t a) -> (a -> t (m b)) -> m (t b)
bind2V  m f = join $ fmap (fmap join . sequenceA . fmap              f ) . sequenceA $ m :: t (m a) -> (a -> m (t b)) -> m (t b)
bind2FV m f = join $ fmap (fmap join . sequenceA . fmap (sequenceA . f)) . sequenceA $ m :: t (m a) -> (a -> t (m b)) -> m (t b)
-}
