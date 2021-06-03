module Internal.Moderation where

import Import.NoFoundation
import Streamer


data ParsedMessage = ParsedMessage
  {rawText :: Text
  }


parseMessage :: Text -> ParsedMessage
parseMessage str = ParsedMessage str
