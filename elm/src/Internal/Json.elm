module Internal.Json exposing (..)

import Dict
import Json.Decode as JD
import Json.Encode as JE

import Internal.Internal exposing (..)
import Main.Model exposing (..)



jdFieldType str = flip JD.andThen <| JD.field str JD.string


-- staticInfo
jdDictEmote = JD.map Dict.fromList
 <| JD.map (List.map <| \emote -> (emote.name, emote))
 <| JD.list jdEmote

jdEmote = JD.map2 Emote
  (JD.field "name" JD.string)
  (JD.field "image" jdImage)

jdImage = JD.map3 Image
  (JD.field "width" JD.float)
  (JD.field "height" JD.float)
  (JD.field "url" JD.string)



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- MainUser
jdProfile = JD.map6 ProfileRecord
  (JD.field "account" <| JD.map5 AccountRecord
    (JD.field "email" JD.string)
    (JD.field "num_months_subbed" JD.int)
    (JD.field "season" JD.int)
    (JD.field "twitch_conn" JD.bool)
    (JD.field "google_conn" JD.bool))
  (JD.field "username" JD.string)
  (JD.field "role" <| JD.oneOf
    [JD.map ProfileSpecialRole <| JD.map2 ProfileSpecialRoleRecord
      (JD.field "name" JD.string)
      (JD.field "power" JD.int)
    ,JD.map ProfileChatterRole <| JD.map2 ProfileChatterRoleRecord
      (JD.field "months" JD.int)
      (JD.field "subscription" <| JD.nullable <| JD.map5 SubscriptionRecord
        (JD.field "tier" JD.int)
        (JD.field "gifter" <| JD.nullable JD.string)
        (JD.field "is_3_month_package" JD.bool)
        (JD.field "recurring" JD.bool)
        (JD.field "end_time" JD.int))])
  (JD.field "badges" jdBadges)
  (JD.field "pronouns" <| JD.nullable JD.string)
  (JD.field "name_color" <| JD.map4 ProfileNameColorRecord
    jdDefaultNameColor
    (JD.field "left" <| JD.nullable jdChromaColor)
    (JD.field "right" <| JD.nullable jdChromaColor)
    (JD.field "mode" jdChromaMode))


jdBadges = JD.map3 ProfileBadges
  (JD.field "first_badge" <| JD.nullable JD.string)
  (JD.field "second_badge" <| JD.nullable JD.string)
  (JD.field "collection" <| JD.list JD.string)

jdEquipedBadges = JD.map2 pair
  (JD.field "first_badge" <| JD.nullable JD.string)
  (JD.field "second_badge" <| JD.nullable JD.string)


-- Name Color
jdNameColor = JD.oneOf
  [JD.map Err jdDefaultNameColor_
  ,JD.map Ok jdNameColor_]

jdDefaultNameColor = JD.map2
  (\c b -> if b then Err c else Ok c)
  (JD.field "default_name_color" jdDefaultNameColor_)
  (JD.field "is_random" JD.bool)


jdDefaultNameColor_ = flip JD.map JD.string <| \str -> case str of
  "Blue" -> Blue
  "Azure" -> Azure
  "Sky" -> Sky
  "Turquoise" -> Turquoise
  "Turtle" -> Turtle
  "Green" -> Green
  "Slime" -> Slime
  "Yellow" -> Yellow
  "Bronze" -> Bronze
  "Mocha" -> Mocha
  "Orange" -> Orange
  "DarkRed" -> DarkRed
  "Red" -> Red
  "Rose" -> Rose
  "Pink" -> Pink
  "Purple" -> Purple
  _ -> Lavender

jeDefaultNameColor color = JE.string <| case color of
  Blue -> "Blue"
  Azure -> "Azure"
  Sky -> "Sky"
  Turquoise -> "Turquoise"
  Turtle -> "Turtle"
  Green -> "Green"
  Slime -> "Slime"
  Yellow -> "Yellow"
  Bronze -> "Bronze"
  Mocha -> "Mocha"
  Orange -> "Orange"
  DarkRed -> "DarkRed"
  Red -> "Red"
  Rose -> "Rose"
  Pink -> "Pink"
  Purple -> "Purple"
  Lavender -> "Lavender"


jdNameColor_ = JD.oneOf
  [JD.map (ChromaName << defaultChromaColor) jdDefaultNameColor_
  ,JD.map ChromaName jdChromaColor
  ,JD.map ChromaNameGradient <| JD.map3 ChromaNameGradientRecord
     (JD.field "left" jdChromaColor)
     (JD.field "right" jdChromaColor)
     (JD.field "mode" jdChromaMode)]

jdChromaMode = flip JD.map JD.string <| \str -> case str of
  "LRGB" -> LRGB
  "RGB" -> RGB
  "HSL" -> HSL
  "HSV" -> HSV
  "HSI" -> HSI
  "LCH" -> LCH
  _ -> LAB

jdChromaColor = JD.map5 ChromaColorRecord
  (JD.field "hue" JD.int)
  (JD.field "chroma_light" JD.int)
  (JD.field "chroma_dark" JD.int)
  (JD.field "value_light" JD.int)
  (JD.field "value_dark" JD.int)

jeChromaColor chromaColor = JE.object <|
  [pair "hue" <| JE.int chromaColor.hue
  ,pair "chroma_light" <| JE.int chromaColor.chromaLight
  ,pair "chroma_dark" <| JE.int chromaColor.chromaDark
  ,pair "value_light" <| JE.int chromaColor.valueLight
  ,pair "value_dark" <| JE.int chromaColor.valueDark
  ]

-- Role
jdRole = JD.oneOf
  [JD.field "special" <| JD.map Special JD.string
  ,jdSubscriber]

jdSubscriber = JD.map Subscriber <| JD.map2 SubscriberRecord
  (JD.field "tier" JD.int)
  (JD.field "months" JD.int)



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




streamStatusFlagDecoder = JD.field "streamStatus" <|
  flip JD.andThen (JD.field "streamStatus" JD.string) <| \str ->
    JD.map Just <| case str of
      "streaming" -> JD.map Streaming <| JD.map5 StreamingRecord
         (JD.oneOf
            [JD.field "twitch" <| JD.map TwitchStream JD.string
            ,JD.field "youtube" <| JD.map YouTubeStream JD.string])
         (JD.field "title" JD.string)
         (JD.field "start_time" JD.int)
         (JD.field "up_time" <| JD.succeed Nothing)
         (JD.field "viewer_count" JD.int)
      "hosting" -> JD.map Hosting <| JD.map2 HostingRecord
        (JD.oneOf
           [JD.field "twitch" <| JD.map TwitchStream JD.string
           ,JD.field "youtube" <| JD.map YouTubeStream JD.string])
        (JD.field "title" JD.string)
      "offline" -> JD.succeed Offline
      _ -> JD.fail "Could Not Parse stream_status"




--------------------------------------------------------------------------------

-- for sending over setSettingsStorage
encodeMainUserSettings : SettingsRecord -> String
encodeMainUserSettings settings = JE.encode 0 <| JE.object
  [("theme_mode", JE.bool settings.themeMode)
  ,("show_timestamps", JE.bool settings.showTimestamps)
  ,("show_emotes", JE.bool settings.showEmotes)
  ,("animate_emotes", JE.bool settings.animateEmotes)
  ,("show_badges", JE.bool settings.showBadges)
  ,("style_usernames", case settings.styleUsernames of
                         Just b -> JE.bool b
                         _ -> JE.null)
  ,("text_emphasis", case settings.textEmphasis of
                         Just b -> JE.bool b
                         _ -> JE.null)
  ,("text_size", JE.int settings.textSize)
  ,("mention_ignored_users", JE.bool settings.mentionIgnoredUsers)
  ,("curse_swear_words", JE.bool settings.curseSwearWords)
  ,("nsfw", JE.bool settings.nsfw)
  ,("show_banned_messages", case settings.showBannedMessages of
                            Just b -> JE.bool b
                            _ -> JE.null)
  ]
