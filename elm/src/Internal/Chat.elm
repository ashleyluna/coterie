module Internal.Chat exposing (..)

import Dict
import Parser as P exposing (..)
import Parser.Dict as PD

import Internal.Internal exposing (..)
import Main.Model exposing (..)




parseMessage : CommonInfo -> ChatUserList -> Bool -> String -> ParsedMessage
parseMessage commonInfo users b str = ParsedMessage str <|
  let emoteList = Dict.keys commonInfo.staticInfo.globalEmoteList
               ++ if b then [] else Dict.keys commonInfo.staticInfo.subOnlyEmoteList
      userList = Dict.keys <| flip Dict.union users.chatters <|
        List.foldl (flip Dict.union) Dict.empty <|
        List.concatMap Dict.values users.specialUsers
  in case runMessageParser emoteList userList str of
       Ok parsedMessage -> parsedMessage
       Err _ -> [PText str]


runMessageParser : List String -> List String -> String
                -> Result (List DeadEnd) (List ParserdPiece)
runMessageParser emoteNames usernames str =
  let slice offset1 offset2 = String.slice offset1 offset2 str
      preDictWrapper wrapper = List.map (\keyword -> (keyword, wrapper keyword))
      keywords = PD.fromDict <| Dict.fromList <|
           List.map (\keyword -> (keyword, (True, keyword))) usernames
        ++ List.map (\keyword -> (String.toLower keyword, (False, keyword))) emoteNames
      finish = Done << List.reverse
      messageParser (initialOffSet, parsedSections) = oneOf
         -- end
        [succeed (\initialOffSet2 -> finish <|
                   if initialOffSet == initialOffSet2
                      then parsedSections
                      else PText (slice initialOffSet initialOffSet2)
                        :: parsedSections)
                 |= getOffset
                 |. end
         -- usernames and emotes
        ,succeed (\initialOffSet2 (isUserRef, keyword_) keywordOffset continue_ ->
                   let continue pMsgs = case continue_ of
                         Just newOffset -> Loop (newOffset, pMsgs)
                         _ -> finish pMsgs
                       keyword = if isUserRef
                         then PUserRef keyword_ (slice initialOffSet2 keywordOffset)
                         else PEmote keyword_
                   in continue <| if initialOffSet == initialOffSet2
                        then keyword
                          :: parsedSections
                        else keyword
                          :: PText (slice initialOffSet initialOffSet2)
                          :: parsedSections)
                 |= getOffset
                 |= backtrackable keywords
                 |= getOffset
                 |= oneOf [succeed Just
                                   |= getOffset
                                   |. token " "
                          ,succeed Nothing
                                   |. end]
         -- normal text
        ,succeed (Loop (initialOffSet, parsedSections))
                 |. chompUntilEndOr " "
                 |. oneOf [spaces, end]
        ]
  in run (loop (0,[]) messageParser) <| String.toLower str






--type ParserdPiece = PText String | PUserRef String | PEmote String
--import Parser exposing (..)
--import Parser.Dict as PD
--import Dict
--
--
--runMessageParser ["AshleyLuna"] [] "duck AshleyLuna quack ashleylunagoose words ashleyluna"
