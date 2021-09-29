module Postfoundation.Moderation where

import Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Strict as HashMap

import Import
import Streamer

{-
system commands
  - Ban: \ban username
  - Mute: \mute username
  - Add command: \addcom output
    Syntax:
      {1} = parameter 1, {2} = parameter 2...
      {count} = number of times this command was used (increments even without being used)
-}
--parseSystemCommand :: Text -> Maybe SystemCommand

{-
Messages are NOT meaningful if
  -- it only contains emotes
  -- is not a bot command
  -- contains less than 4 words
Should already be assumed that
  - this is not a system command
-}
isMessageMeaningful :: Text -> Handler Bool
isMessageMeaningful str = if length (words str) < 4 then return False else do
  App {..} <- getYesod
  emoteList <- HashMap.union
    <$> readTVarIO tvGlobalEmoteList
    <*> readTVarIO tvSubOnlyEmoteList
  case Atto.parse parser str of
    Atto.Done _ _ -> return True
    _ -> return False
  where parser = do
          return True



data SystemCommand
  = BanCommand
  | MuteCommand

data BotCommand
  = BotPost Text -- output of the bot command
  | BotParam (Text -> BotCommand) -- command takes a parameter before returning output (could require more params)