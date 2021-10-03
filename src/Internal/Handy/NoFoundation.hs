{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.Handy.NoFoundation where

import ClassyPrelude.Yesod

import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import Data.Text as Text
import qualified Data.List as List
import Data.Time.Clock.POSIX as POSIX
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)

import Streamer

type WSConnId = Int

type TVHashMap k v = TVar (HashMap k v)
type TVHash64Map v = TVar (HashMap Int64 v)
type TVHash64MapTV v = TVar (HashMap Int64 (TVar v))
type TVIntMap v = TVar (IntMap v)


type Cont r a = (a -> r) -> r

flip2 f b c a = f a b c

f2map :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f2map = fmap . fmap

intmap f as =
  let intmap_ n f (a:as) = f n a : intmap_ (n + 1) f as
      intmap_ _ _ [] = []
  in intmap_ 1 f as

pair a b = (a,b)
dup a = (a,a)

isLeft e = case e of
  Left _ -> True
  _ -> False
isRight e = case e of
  Right _ -> True
  _ -> False

whem :: Monoid m => Bool -> m -> m
whem b m = if b then m else mempty

showText :: Show a => a -> Text
showText = Text.pack . show

-- including each num
between :: Ord a => a -> a -> a -> Bool
between num1 num2 n = n >= num1
                   && n <= num2

betweenLen :: Int -> Int -> Text -> Bool
betweenLen num1 num2 str = Text.length str >= num1
                        && Text.length str <= num2

--validUsername :: Text -> Bool
--validUsername username = betweenLen 3 24 username
--                     && Text.all (\c -> Char.isAlphaNum c || c == '_')
--                                 username

--validPronouns


mkQuery :: a -> b -> (a, Maybe b)
mkQuery a b = (a, Just b)

-- in micro seconds
getCurrentTime :: MonadIO m => m Int
getCurrentTime = round . (* 1000000) <$> liftIO POSIX.getPOSIXTime

rfc3339ToPOSIXSeconds :: Text -> Maybe Int
rfc3339ToPOSIXSeconds time = round . utcTimeToPOSIXSeconds . zonedTimeToUTC <$>
  parseTimeRFC3339 time

jsonResponse :: Monad m => Text -> [(Text, Value)] -> m Value
jsonResponse responseType json = return $ object $
  ("type" .= responseType) : json


jsonError :: Monad m => Text -> m Value
jsonError errMsg = return $ object ["error" .= errMsg]

-- in micro seconds
seconds :: Num a => a
seconds = 1000000
minutes :: Num a => a
minutes = 60 * seconds
hours :: Num a => a
hours = 60 * minutes
days :: Num a => a
days = 24 * hours

sessionTimeOut :: Num a => a
sessionTimeOut = 30 * 24 * 60 -- 30 days

--printUrl = liftIO . putStrLn =<< getUrlRender <*> return HomeR

assignSeason :: Int -> Int
assignSeason time = List.foldr
  (\seasonEndTime a -> if seasonEndTime > time then a else a + 1)
  1 (seasonEndTimes streamerInfo)



modifyTVarIO :: MonadIO m => TVar a -> (a -> a) -> m ()
modifyTVarIO tvar = atomically . modifyTVar' tvar




