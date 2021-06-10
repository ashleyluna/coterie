module Page.Home exposing (..)

import Array
import Browser.Dom
import Css
import Css.Animations as CssA
import Css.Transitions as CssT
import Dict
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Lazy as HtmlL
import Iso8601
import Task
import Time exposing (Posix, Zone)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La

import List.Extra

import Chat.ChatBox exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)
import Update.Page exposing (..)
import Update.CommonInfo exposing (..)
import Update.LiveInfo exposing (..)






homePage model homePageInfo =
  let colorPalette = currentColorPalette model
      mainChat = model.liveInfo.mainChat
  in finishedLayout model <| co
       [width fill, height fill]
       [navBar model homePageInfo
       ,ro [width fill, height fill]
           [mainBox model homePageInfo
           ,el [width <| px 500, height fill] <|
               Element.map (HomePageMsg << HomeChatBoxMsg) <|
               chatBox model MainChat "homepage-" mainChat homePageInfo.chatBox]]













navBar : Model -> HomePageInfo -> Element Msg
navBar model homePageInfo =
  let colorPalette = currentColorPalette model
      settings = model.commonInfo.settings
      logoShrink = homePageInfo.streamScreenSize
      divider tall = em [width <| px 2, height <| px tall, alignRight
                        ,Bg.color colorPalette.txSoft]
  in ro [width fill, paddingXY 12 0, spacing 4
        ,Ft.size 18, Ft.color colorPalette.txSoft
        ,Bg.color colorPalette.bgMain
        ,Bdr.color colorPalette.txSoft
        ,htmlStyle "user-select" "none"] <|
        -- Logo Space (not actuallt the logo)
        [let logoHeight = if logoShrink then 40 else 120
             logoWidth = round <| 8 + (streamerInfo.logo.width *
                                       logoHeight / streamerInfo.logo.height)
         in em [paddingEach {right = if logoShrink then 0 else -16
                            ,left = 0, top = 0, bottom = 0}
               --,pointer--, Evt.onClick <| SetFullIFrame <| not model.fullIFrame
               ,width <| px <| if logoShrink then logoWidth else 0
               ,height <| px 40
               ,htmlClass "logoWrapper"
               ,htmlStyle "transition" "width 0.75s ease-in-out, padding 0.75s ease-in-out"
               ,htmlStyle "-webkit-transition" "width 0.75s ease-in-out, padding 0.75s ease-in-out"
               ]
        ]
        ++
        List.map (\pLink -> mkLink [Ft.size 18] pLink.url <| bubbleMain model pLink.title)
                 streamerInfo.navBarLinks
        ++
        [subPageButton model homePageInfo initialHomeSubPageSupportInfo "Support"
        ,subPageButton model homePageInfo initialHomeSubPageDonateInfo "Donate"
        ,subPageButton model homePageInfo initialHomeSubPageSubscribeInfo "Subscribe"
        ,ro [width fill, spacing 12]
            [divider 24
            ,let maxStreamTitleLength = 1000
                 titleScrollTime = (homePageInfo.streamTitleLength - maxStreamTitleLength) / 100 -- 1 second per 100px
             in el [height <| px 48, Ft.size 18, Ft.bold
                   ,Evt.onMouseEnter <| HomePageMsg <| HoverTitle True
                   ,Evt.onMouseLeave <| HomePageMsg <| HoverTitle False
                   ,htmlStyle "cursor" "default"]
             -- wrapper for the title bar, controls the slding when title changes
             <| el [height fill, alignRight, clipX
                   ,htmlStyle "width" <| if homePageInfo.streamTitleLength >= maxStreamTitleLength
                                            then String.fromInt maxStreamTitleLength ++ "px"
                                            else String.fromInt (round homePageInfo.streamTitleLength) ++ "px"
                   ,htmlStyle "transition" "width 2s ease-in-out"
                   ,htmlStyle "-webkit-transition" "width 2s ease-in-out"]
                -- wrapper for the title tx, controls hover scrolling
             <| ex [htmlID "stream-title", centerY
                   ,moveLeft <| if homePageInfo.hoverTitle && homePageInfo.streamTitleLength >= maxStreamTitleLength
                                   then homePageInfo.streamTitleLength - maxStreamTitleLength else 0
                   ,htmlStyle "transition" <| "transform " ++ String.fromFloat titleScrollTime ++ "s linear 0.1s"
                   ,htmlStyle "-webkit-transition" <| "transform " ++ String.fromFloat titleScrollTime ++ "s linear 0.1s"]
                -- the title tx
             <| case model.liveInfo.streamStatus of
                  Just (Streaming info) -> info.title
                  Just (Hosting info) -> "Hosting: " ++ info.title
                  Just Offline -> "Offline"
                  Nothing -> ""
            ,divider 24
            ,let viewCountLength = case model.liveInfo.streamStatus of
                   Just (Streaming _) -> 90
                   _ -> 2
                 titleScrollTime = 102 / 300 -- 1 second per 100px
             in el [height <| px 48, Ft.size 16, Ft.bold
                   ,pointer, Evt.onClick <| HomePageMsg <| NextCounterPosition]
             -- wrapper for the count bar, controls the slding when stream status changes
             <| el [height fill, alignRight, clipX
                   ,htmlStyle "width" <| String.fromInt viewCountLength ++ "px"
                   ,htmlStyle "transition" "width 2s ease-in-out"
                   ,htmlStyle "-webkit-transition" "width 2s ease-in-out"]
             -- wrapper for the count tx, controls hover scrolling
             <| el [centerY
                   ,moveLeft <| if homePageInfo.counterPosition == 0 then 0 else 102
                   ,htmlStyle "transition" <| "transform " ++ String.fromFloat titleScrollTime ++ "s linear 0.05s"
                   ,htmlStyle "-webkit-transition" <| "transform " ++ String.fromFloat titleScrollTime ++ "s linear 0.05s"]
             -- the count tx
             <| case model.liveInfo.streamStatus of
                     Just (Streaming streamInfo) ->
                       ro [spacing 12, Ft.family [Ft.monospace]]
                          [el [width <| px 90]
                           <| ro [centerX]
                                 [faIcon [] "fas fa-user" --tx "$$" -- TODO Viewers number symbol neeeds to be 20 pixels wide and 16 tall
                                 ,tx <| " " ++ String.fromInt streamInfo.viewerCount]
                          ,case streamInfo.upTime of
                                Nothing -> Element.none
                                Just upTime ->
                                  let doubleDigit numStr =
                                        if String.length numStr == 1
                                           then "0" ++ numStr else numStr
                                  in el [width <| px 90]
                                  <| ro [centerX]
                                        [tx <| String.fromInt <| upTime // (60^2)
                                        ,tx ":"
                                        ,tx <| doubleDigit <| String.fromInt <| remainderBy 60 <| upTime // 60
                                        ,tx ":"
                                        ,tx <| doubleDigit <| String.fromInt <| remainderBy 60 upTime]]
                     _ -> em [width <| px 90]
            ]
        ]










mainBox : Model -> HomePageInfo -> Element Msg
mainBox model homePageInfo =
  let colorPalette = currentColorPalette model
      slideStyle position direction initiateSlide =
        let slideAmount = case position of
              Just False -> if not initiateSlide then "-100%"
                else if direction then "-200%" else "0%" -- left
              Nothing -> if not initiateSlide then "0%"
                else if direction then "-100%" else "100%" -- middle
              Just True -> if not initiateSlide then "100%"
                else if direction then "0%" else "200%" -- right
        in [htmlStyle "transform" <| "translate("++slideAmount++",0%)"
           ,htmlStyle "-webkit-transform" <| "translate("++slideAmount++",0%)"
           ]
      fadeStyle fade initiateSlide =
           (if initiateSlide
               then [alpha <| if fade then 1 else 0]
               else [alpha <| if fade then 0 else 1
                    ,htmlStyle "transition" <| if fade
                      then "opacity 0.375s, transform 0.75s" else "transform 0.75s"
                    ,htmlStyle "-webkit-transition" <| if fade
                      then "opacity 0.375s, transform 0.75s" else "transform 0.75s"
                    ])
      wrapper fade position back mainBoxPage = el
        ([width fill, height fill]
         ++ slideStyle position homePageInfo.subPageSlideDirection homePageInfo.initiateSubPageSliding
         ++ fadeStyle fade homePageInfo.initiateSubPageSliding)
        <| mainBoxPage model homePageInfo back
      wrapperL = wrapper True  (Just False) (List.length homePageInfo.history > 1) -- history (Left)
      wrapperM = wrapper False Nothing      (List.length homePageInfo.history > 0) -- current (Middle)
      wrapperR = wrapper True  (Just True)  True                                 -- unappendHistory (Right)
      mkSubPage wrap subPage = case subPage of
        Just (MainMainBoxPage) ->            wrap <| (\model_ homePageInfo_ _ ->
                                                     mainMainBoxPage model_ homePageInfo_)
        Just (HomeSubPageSupport info) ->    wrap <| supportMainBoxPage    info
        Just (HomeSubPageDonate info) ->     wrap <| donateMainBoxPage     info
        Just (HomeSubPageSubscribe info) ->  wrap <| subscribeMainBoxPage  info
        Just (HomeSubPageSubscribe2 info) -> wrap <| subscribe2MainBoxPage info
        Just (HomeSubPageAgreement) ->       wrap <| agreementMainBoxPage
        _ -> Element.none
  in ro [width fill, height fill
        ,Bg.color colorPalette.bgMain
        ,htmlStyle "z-index" "1"
        ,inFront <| streamScreen model homePageInfo
        ,inFront <| logo model homePageInfo
        ]
        [sidePanel model homePageInfo
        ,el [width fill, height fill, clip
            ,Bg.color colorPalette.bgMain
            ,behindContent <| mkSubPage wrapperL <| List.head homePageInfo.history
            ,behindContent <| mkSubPage wrapperR <| homePageInfo.unappendHistory
            ] <|
            mkSubPage wrapperM <| if homePageInfo.streamScreenSize
              then Nothing else Just homePageInfo.subPage
        ]




logo : Model -> HomePageInfo -> Element Msg
logo model homePageInfo =
  let logoImage = streamerInfo.logo
      logoShrink = homePageInfo.streamScreenSize
      logoHeight = if logoShrink then 40 else 120
      logoWidth = round <| 8 + (logoImage.width * logoHeight / logoImage.height)
  in Element.html <| Html.img
       ([HtmlA.class "logo"
        ,HtmlA.style "height" <| String.fromInt logoHeight ++ "px"
        ,HtmlA.style "width" <| String.fromInt logoWidth ++ "px"
        ,HtmlA.style "margin-top" <| if logoShrink then "-44px" else "12px"
        ,HtmlA.style "margin-left" <| if logoShrink then "12px"
          else String.fromInt (round <| 182 - 0.5 * toFloat logoWidth) ++ "px"
        ,HtmlA.style "transition" "width 0.75s ease-in-out, height 0.75s ease-in-out, margin 0.75s ease-in-out"
        ,HtmlA.style "-webkit-transition" "width 0.75s ease-in-out, height 0.75s ease-in-out, margin 0.75s ease-in-out"
        ,HtmlA.style "z-index" "1"
        ,HtmlA.attribute "src" logoImage.url
        ,HtmlA.attribute "alt" "LogoImage"
        ] ++
        if List.member model.liveInfo.streamStatus [Nothing, Just Offline]
           then []
           else [HtmlA.style "cursor" "pointer"
                ,HtmlE.onClick <| HomePageMsg <|
                   if homePageInfo.streamScreenSize || isSubPageOn homePageInfo.subPage initialMainMainBoxPageInfo
                      then SetStreamScreenSize <| not logoShrink
                      else SetSubPageDefault True
                ])
       []
          --{src = logoImage.link
          --image
          --[height <| px logoHeight
          --,width <| px logoWidth
          --,htmlStyle "margin-top" <| if model.fullIFrame
          --  then "0px" else "56px"
          --,htmlStyle "margin-left" <| if model.fullIFrame
          --  then "0px" else String.fromInt (round <| 170 - 0.5 * toFloat logoWidth) ++ "px"
          --,htmlStyle "transition" "width 2s ease-in-out, height 2s ease-in-out, margin 2s ease-in-out"
          --,htmlStyle "-webkit-transition" "width 2s ease-in-out, height 2s ease-in-out, margin 2s ease-in-out"
          --,htmlStyle "z-index" "1"
          --]
          --{src = logoImage.link
          --,description = "LogoImage"}]




streamScreen : Model -> HomePageInfo -> Element Msg
streamScreen model homePageInfo =
  let colorPalette = currentColorPalette model
      screenShrinkSize = if isStreamOffline model then Nothing
        else Just <| homePageInfo.streamScreenSize
      twitchStream channel = Element.html <| Html.iframe
        ([HtmlA.src <| "https://player.twitch.tv/?parent=localhost&channel=" ++ channel
         ,HtmlA.style "width" "100%"
         ,HtmlA.style "height" "100%"
         ,HtmlA.style "z-index" "1"
         ,HtmlA.attribute "scrolling" "no"
         ,HtmlA.attribute "frameborder" "0"
         ,HtmlA.attribute "allowfullscreen" "true"]
        ++ if screenShrinkSize == Just True then []
              else [HtmlA.style "pointer-events" "none"])
        []
  in el ([Bg.color colorPalette.bgMain
         ,htmlStyle "transition" "width 0.75s ease-in-out, height 0.75s ease-in-out, margin 0.75s ease-in-out"
         ,htmlStyle "-webkit-transition" "width 0.75s ease-in-out, height 0.75s ease-in-out, margin 0.75s ease-in-out"
         ] ++
         case screenShrinkSize of
           Just True -> [htmlStyle "width" "100%"
                        ,htmlStyle "height" "100%"
                        ,htmlStyle "margin-top" "0px"
                        ,htmlStyle "margin-left" "0px"]
           Just False -> [htmlStyle "width" "340px"
                         ,htmlStyle "height" "191px"
                         ,htmlStyle "margin-top" "160px"
                         ,htmlStyle "margin-left" "16px"
                         ,pointer
                         ,Evt.onClick <| HomePageMsg <| SetStreamScreenSize True
                         ,inFront <| el ([width fill, height fill, Ft.size 24
                                         ,htmlStyle "z-index" "1"
                                         ,Ft.color <| rgba255 220 220 220 0
                                         ,Bg.color <| rgba255 0 0 0 0
                                         ,mouseOver [Ft.color <| rgba255 255 255 255 1
                                                     ,Bg.color <| rgba255 0 0 0 0.7]]
                                         ++ colorTransitionStyle)
                                  <| ex [centerX, centerY]
                                        "Expand"]
           _ -> [htmlStyle "width" "0px"
                ,htmlStyle "height" "0px"
                ,htmlStyle "margin-top" "160px"
                ,htmlStyle "margin-left" "16px"])
        <| case model.liveInfo.streamStatus of
             Just (Streaming info) ->
               case info.stream of
                    TwitchStream channel -> twitchStream channel
                    YouTubeStream channel -> el [width fill, height fill] Element.none -- TODO
             Just (Hosting info) ->
               case info.stream of
                    TwitchStream channel -> twitchStream channel
                    YouTubeStream channel -> el [width fill, height fill] Element.none -- TODO
             _ -> el [width fill, height fill] Element.none


sidePanel model homePageInfo =
  let colorPalette = currentColorPalette model
      overlayButton subPageInfo = el
            [Ft.size 18
            ,Evt.onClick <| HomePageMsg <| if isSubPageOn homePageInfo.subPage subPageInfo
              then UnappendSubPage
              else AppendSubPage subPageInfo] <<
            bubbleActiveMain model (isSubPageOn homePageInfo.subPage subPageInfo)
  in em [width <| px 372, height fill
        ,htmlStyle "margin-top" "351px"]








donateMainBoxPage : DonateRecord -> Model -> HomePageInfo -> Bool -> Element Msg
donateMainBoxPage subPageInfo model homePageInfo back =
  let colorPalette = currentColorPalette model
      setDonate = HomePageMsg << UpdateSubPage << HomeSubPageDonate
  in mainBoxPageWrapper model <|
       co [width fill, height fill, spacing 64
          ,Ft.size 16, Ft.bold, noSelection]
          [mainBoxPageTitle model back "Donate"
          ,co [width <| maximum 800 fill, centerX, spacing 24, Bdr.rounded 16] <|
              [el [width fill, paddingXY 32 0] <|
               In.text (fieldStyle model) -- rgb255 254 204 79
                       {onChange = setDonate << \str -> {subPageInfo | name = str}
                       ,text = subPageInfo.name
                       ,placeholder = placeholder (Ft.color colorPalette.txSoft2) "Name"
                       ,label = In.labelHidden "donatorNameInput"}
              ,el [width fill, paddingXY 32 0] <|
               In.text (fieldStyle model) -- rgb255 254 204 79
                       {onChange = setDonate << \str -> {subPageInfo | amount = str}
                       ,text = subPageInfo.amount
                       ,placeholder = placeholder (Ft.color colorPalette.txSoft2) "Amount"
                       ,label = In.labelHidden "donationAmountInput"}
              ,el [width fill, paddingXY 32 0] <|
               In.multiline [width fill, Ft.color colorPalette.txSoft
                            ,paddingXY 10 10
                            ,Bdr.width 2, Bdr.rounded 8, Bdr.color colorPalette.bgMainDark
                            ,Bg.color colorPalette.bgMainDark
                            ,focused [Bdr.color colorPalette.mainHighlight]] -- rgb255 254 204 79
                            {onChange = setDonate << (\str ->
                               if String.length str <= 250
                                  then {subPageInfo | message = str}
                                  else {subPageInfo | message = String.left 250 str})
                            ,text = subPageInfo.message
                            ,placeholder = Just <| In.placeholder [] <| tx "Donation Message, Max 250 Characters"
                            ,label = In.labelHidden "donationMessageInput"
                            ,spellcheck = True}]
              --,ro [width fill, alignBottom, paddingXY 32 0, spaceEvenly]
              --    [el ([width <| px 160, paddingXY 0 8
              --         ,Ft.size 32, Ft.color colorPalette.txSoft
              --         ,Bg.color colorPalette.bgMainVeryDark
              --         ,Bdr.rounded 16
              --         ,mouseOver [Ft.color colorPalette.bgMainVeryDark
              --                    ,Bg.color highlightRed]
              --         ,Evt.onClick <| setDonate subPageInfo]
              --         ++ buttonStyle
              --         ++ colorTransitionStyle) <|
              --         el [centerX] <| tx "Back"
              --    ,el ([width <| px 160, alignRight, paddingXY 0 8
              --         ,Ft.size 32, Ft.color colorPalette.txSoft
              --         ,Bg.color colorPalette.bgMainVeryDark
              --         ,Bdr.rounded 16
              --         ,mouseOver [Ft.color colorPalette.bgMainVeryDark
              --                    ,Bg.color highlightBlue
              --                    ,Bdr.color highlightBlue]
              --         --,Evt.onClick <| setDonate {subPageInfo | page2Info = Nothing}
              --         ]
              --         ++ buttonStyle
              --         ++ colorTransitionStyle) <|
              --         el [centerX] <| tx "Confirm"]]
          ,co [width <| maximum 800 fill, centerX, alignBottom, padding 32, spacing 48]
              [el ([centerX, paddingXY 16 8
                   ,Ft.size 32
                   ,Ft.color colorPalette.txSoft
                   ,Bg.color colorPalette.bgMain
                   ,Bdr.rounded 16
                   ,mouseOver [Ft.color colorPalette.bgMain
                              ,Bg.color colorPalette.highlightBlue
                              ,Bdr.color colorPalette.highlightBlue]
                   ,Evt.onClick <| HomePageMsg <| POSTDonation subPageInfo]
                   ++ buttonStyle
                   ++ colorTransitionStyle) <|
               ex [centerX] "Confirm"
              ,paymentConditions model]
          ]








subscribeMainBoxPage : SubscribeRecord -> Model -> HomePageInfo -> Bool -> Element Msg
subscribeMainBoxPage subPageInfo model homePageInfo back =
  let colorPalette = currentColorPalette model
      setSubscribe = HomePageMsg << UpdateSubPage << HomeSubPageSubscribe
      tieredBenefit tier = ex
        ([centerX]
        ++ if subPageInfo.tier >= tier
              then [Ft.color colorPalette.txSoft]
              else [Ft.color colorPalette.txSoft2, Ft.strike])
      recipientSelector recipient = el
         [centerX, Ft.size 18
         ,Evt.onClick <| setSubscribe {subPageInfo | recipient = recipient}] <<
          bubbleActiveMain model (subPageInfo.recipient == recipient)
      -- TODO This will not be used until points system works
      --earnPointsReward =
      --  let points = List.intersperse (tx " ") <|
      --        flip List.indexedMap ["x1.5","x2","x2.5","x3"] <|
      --          \int -> tx >> if subPageInfo.tier == int + 1
      --            then el [Ft.color colorPalette.mainHighlight]
      --            else el [Ft.color colorPalette.txSoft2, Ft.strike]
      --   in co [centerX, Ft.color colorPalette.txSoft, spacing 6]
      --         [ro [centerX]
      --          <| [tx "+ Earn "] ++ points ++ [tx " Points"]
      --         ,el [centerX] <| tx "For Fun Rewards"]
  in mainBoxPageWrapper model <| co
       [width fill, height fill, spacing 24, clipX
       ,Ft.size 16, Ft.bold, noSelection]
       [mainBoxPageTitle model back "Subscribe"
       ,el [width fill, paddingXY 0 20] <|
        slidingHeader model subPageInfo.tier
          homePageInfo.subPageHeaderSlideDirection
          homePageInfo.initiateSubPageHeaderSliding
          (\int -> HomeSubPageSubscribe {subPageInfo | tier = int})
          (\b1 b2 o -> HomePageMsg <| SlideSubPageHeader b1 b2 o)
          (flip List.map (List.range 1 <| Array.length streamerInfo.subscriptionTiersLogos)
                ((++) "Tier " << String.fromInt))
       ,case Array.get (subPageInfo.tier - 1) streamerInfo.subscriptionTiersLogos of
          Just subTier -> ex [centerX
                             ,Ft.size 24, Ft.color <| colorPalette.txSoft]
                             subTier.name
          _ -> el [height <| px 24] Element.none
       ,subTierImage model subPageInfo.tier
       ,ro [width <| maximum 600 fill, centerX, Ft.size 15, Ft.regular]
        <| List.map (co [width <| fillPortion 1, spacing 24])
           [[ro [centerX, spacing 8]
                [ex [centerX, Ft.color <| colorPalette.txSoft] "+ Fancy Badge"
                ,let allSubBadges = model.commonInfo.staticInfo.subBadges
                     defaultBadge = case Maybe.andThen List.head <| Array.get 0 allSubBadges of
                       Just badge -> mkEmote 18 badge.emote
                       _ -> Element.none
                 in case Maybe.map .role model.commonInfo.profile of
                      Just (ProfileChatterRole chatterRole) -> case Array.get (subPageInfo.tier - 1) allSubBadges of
                        Just badgeList ->
                          let subBadge = List.head <| Tuple.first <|
                                List.partition ((<=) chatterRole.months << .monthsRequired) badgeList
                          in case subBadge of
                               Just badge -> mkEmote 18 badge.emote
                               _ -> defaultBadge
                        _ -> defaultBadge
                      _ -> defaultBadge
                ,ex [Ft.color colorPalette.mainHighlight] "Username"]
            ,tieredBenefit 2 "+ Custom Username color"
            ]
           ,[ro [centerX, spacing 4, Ft.color <| colorPalette.txSoft]
                [tx "+ Use Subscriber Only Emotes In Chat"
                ,mkEmote 18 streamerInfo.displayEmote]
            ,tieredBenefit 3 "+ Add A Gradient To Your Username"
            --,ro [centerX]
            --    [ex [Ft.color colorPalette.txSoft] "+ Color"
            --    ,ex [Ft.color colorPalette.txSoft2] " | "
            --    ,ex [Ft.color colorPalette.mainHighlight] "tx"
            --    ,ex [Ft.color colorPalette.txSoft2] " | "
            --    ,ex [Ft.color colorPalette.txSoft] " using vertial bars"]
            ]
           ]
       -- Recipient Selectors
       ,ro [width <| maximum 700 fill, centerX, spacing 40, paddingXY 0 24]
           [recipientSelector Positive
              "Your Account"
           ,recipientSelector Neutral
              "Gift A Friend"
           ,recipientSelector Negative
              "Random Gifts"
           ]
       ,case subPageInfo.recipient of
          Positive -> ro
            [centerX, paddingXY 0 10, spacing 14]
            [el [Ft.size 20
                ,Ft.color colorPalette.txSoft] <| tx "Auto-Renew"
            ,em []  -- this extra el is here because the up arrow and auto renew color transitions are conflicting
            ,buttonSlider model subPageInfo.autoRenew <|
               setSubscribe {subPageInfo | autoRenew = not subPageInfo.autoRenew}
            ]
          Neutral -> co
            [centerX, spacing 20, Ft.size 16] <|
            [el [width shrink, centerX] <|
             fieldCheckMain model In.username
               subPageInfo.inValidGiftFriendSearch
               (setSubscribe << \str -> {subPageInfo | giftFriendSearch = str})
               (\str -> HomePageMsg <| GiftFriendCheck str)
               subPageInfo.giftFriendSearch
               "User"
               "Subscription Gift User Search Input"
             ,em []  -- this extra el is here because the up arrow and auto renew color transitions are conflicting
             ,elIf (subPageInfo.inValidGiftFriendSearch >= 2) <|
                   fieldErrorText model <| case subPageInfo.inValidGiftFriendSearch of
                     2 -> "User does not exist"
                     3 -> "You cannout gift yourself a subscription"
                     _ -> "This user cannot be gifted a subscription"
             ]
          _ -> ro
            [centerX, spacing 16, Ft.size 16]
            [In.text (fieldStyle model)
                     {onChange = \str -> setSubscribe <|
                        case String.toInt str of
                             Just int -> {subPageInfo | numberOfRandomGiftSubs = if int < 1 then 1 else int}
                             _ -> subPageInfo --,Evt.onClick SearchUserForGiftSubscription TODO
                     ,text = String.fromInt subPageInfo.numberOfRandomGiftSubs
                     ,placeholder = placeholder (Ft.color colorPalette.txSoft2) ""
                     ,label = In.labelHidden "Random Subscription Gift Input"}
            ,em []  -- this extra el is here because the up arrow and auto renew color transitions are conflicting
            ,co [Ft.size 18, Ft.color colorPalette.txSoft]
             <| let arrowButton num = faIcon <|
                      [width fill, height fill, paddingXY 4 0
                      ,Bdr.rounded 6
                      ,pointer, mouseOver [Bg.color colorPalette.bgMain3]
                      ,Evt.onClick <| setSubscribe <| {subPageInfo | numberOfRandomGiftSubs = num}]
                      ++ colorTransitionStyle
                in [arrowButton (min 100 <| subPageInfo.numberOfRandomGiftSubs + 1)
                                "fas fa-chevron-up"
                   ,arrowButton (max 1 <| subPageInfo.numberOfRandomGiftSubs - 1)
                                "fas fa-chevron-down"]]
       -- Month Package Selectors
       ,ro [width fill, centerX, alignBottom
           ,paddingEach {bottom = 48, left = 128, right = 128, top = 0}] <|
           let (singleMonth, package, packageFull) = subCost subPageInfo.tier <|
                 if subPageInfo.recipient == Negative
                    then subPageInfo.numberOfRandomGiftSubs else 1
               monthPackageSelectionPiece msg month content =
                 el [width <| fillPortion 1] <|
                 el ([width <| px 120, height <| px 80, centerX
                     ,Ft.color colorPalette.txSoft
                     ,Bdr.rounded 16
                     ,noSelection]
                    ++ colorTransitionStyle
                    ++ listIf (subPageInfo.recipient /= Neutral || subPageInfo.inValidGiftFriendSearch == 1)
                               [pointer
                               ,mouseOver [Ft.color colorPalette.bgMainVeryDark
                                          ,Bg.color colorPalette.highlightBlue
                                          ,Bdr.color colorPalette.highlightBlue]
                               ,Evt.onClick <| HomePageMsg <| AppendSubPage <| HomeSubPageSubscribe2
                                 {tier = subPageInfo.tier, recipient = subPageInfo.recipient
                                 ,autoRenew = subPageInfo.autoRenew, giftFriendSearch = subPageInfo.giftFriendSearch
                                 ,numberOfRandomGiftSubs = subPageInfo.numberOfRandomGiftSubs
                                 ,month3Package = msg, message = ""}]) <|
                 co [width fill, height fill, centerX, padding 8]
                    [content
                    ,ex [centerX, alignBottom, Ft.size 24] month]
           in [monthPackageSelectionPiece False "1 Month" <|
                  ex [centerX, Ft.size 28] <| "$" ++ String.fromInt singleMonth
              ,monthPackageSelectionPiece True "3 Months" <|
                 ro [centerX, Ft.size 28]
                    [ex [Ft.size 16, Ft.strike] <| "$" ++ String.fromInt packageFull
                    ,tx <| "$" ++ String.fromInt package
              ]]


      ---- Recipient
      --,ro [width <| px 700, centerX, paddingTop 24, spaceEvenly] <|
      --    [el [width <| fillPortion 1, alignTop] <|
      --     co [width fill, centerX, spacing 16]
      --        [el [width fill, height <| px 44, clip] <|
      --         el [centerX
      --            ,htmlStyle "bottom" <| if subPageInfo.recipient == Positive then "0px" else "44px"
      --            ,htmlStyle "transition" "bottom 1s ease-in-out"
      --            ,htmlStyle "-webkit-transition" "bottom 1s ease-in-out"]
      --             ]]
      --    ,el [width <| fillPortion 1, alignTop] <|
      --     co [width fill, centerX, spacing 16]
      --        [el [width fill, height <| px 44, clip] <|
      --         el [centerX
      --            ,htmlStyle "bottom" <| if subPageInfo.recipient == Neutral then "0px" else "44px"
      --            ,htmlStyle "transition" "bottom 1s ease-in-out"
      --            ,htmlStyle "-webkit-transition" "bottom 1s ease-in-out"]

      --         ]
      --    ,el [width <| fillPortion 1, alignTop] <|
      --     co [width fill, centerX, spacing 16]
      --        [el [width fill, height <| px 44, centerX, clip] <|
      --         el [htmlStyle "bottom" <| if subPageInfo.recipient == Negative then "0px" else "44px"
      --            ,htmlStyle "transition" "bottom 1s ease-in-out"
      --            ,htmlStyle "-webkit-transition" "bottom 1s ease-in-out"] <|
      --        ]
       --    ]
       ]




subscribe2MainBoxPage : Subscribe2Record -> Model -> HomePageInfo -> Bool -> Element Msg
subscribe2MainBoxPage subPageInfo model homePageInfo back =
  let colorPalette = currentColorPalette model
      setSubscribe = HomePageMsg << UpdateSubPage << HomeSubPageSubscribe2
  in mainBoxPageWrapper model <| co
       [width fill, height fill, spacing 24
       ,Ft.size 16, Ft.bold, noSelection]
       [mainBoxPageTitle model True "Subscription Confirmation"
       ,co [width <| maximum 800 fill, centerX, spacing 12, Bdr.rounded 16] <|
           [case subPageInfo.recipient of
              Positive -> ex
                [paddingBottom 44
                ,centerX, Ft.size 20, Ft.color colorPalette.txSoft] <|
                "Tier " ++ String.fromInt subPageInfo.tier ++ " Subscription"
              Neutral -> co
                [width fill, spacing 20, Ft.size 24, Ft.color colorPalette.txSoft] <|
                [ex [centerX] <| "Tier " ++ String.fromInt subPageInfo.tier ++ " Subscription"
                ,ex [centerX] <| "Gifting To " ++ subPageInfo.giftFriendSearch]
              Negative -> co
                [width fill, spacing 12, Ft.size 24, Ft.color colorPalette.txSoft] <|
                [ex [centerX] <| "Gifting " ++ String.fromInt subPageInfo.numberOfRandomGiftSubs ++ " Tier " ++ String.fromInt subPageInfo.tier
                ,ex [centerX] "Subscriptions To The Community"]
           ,subTierImage model subPageInfo.tier
           ,el [width fill, paddingXY 64 0] <|
            In.multiline [width fill, Ft.color colorPalette.txSoft
                         ,paddingXY 10 10
                         ,Bdr.width 2, Bdr.rounded 8, Bdr.color colorPalette.bgMainDark
                         ,Bg.color colorPalette.bgMainDark
                         ,focused [Bdr.color colorPalette.mainHighlight]] -- rgb255 254 204 79
                         {onChange = setSubscribe << (\str ->
                            if String.length str <= 250
                               then {subPageInfo | message = str}
                               else {subPageInfo | message = subPageInfo.message})
                         ,text = subPageInfo.message
                         ,placeholder = Just <| In.placeholder [] <| tx "Subscription Message, Max 250 Characters"
                         ,label = In.labelHidden "Subscription Gift User Search Input"
                         ,spellcheck = True}]
           ,case subPageInfo.recipient of
                 Positive -> if subPageInfo.autoRenew
                                then ex [centerX, Ft.color colorPalette.txSoft] <|
                                        if subPageInfo.month3Package
                                                then "Renew Subscription Every 3 Months"
                                                else "Renew Subscription Every Month"
                                else ex [centerX, Ft.color colorPalette.txSoft2]
                                        "Do Not Renew Subscription"
                 _ -> Element.none
           ,ro [centerX] <|
               [ex [Ft.size 32,  Ft.color colorPalette.txSoft] <|
                   "$"
                   ++ (String.fromInt <|
                   (if subPageInfo.recipient == Negative
                       then (*) subPageInfo.numberOfRandomGiftSubs else identity) <|
                       case (subPageInfo.tier, subPageInfo.month3Package) of
                            (1,False) -> 5
                            (1,True) -> 12
                            (2,False) -> 10
                            (2,True) -> 24
                            (3,False) -> 20
                            (3,True) -> 48
                            (_,False) -> 40
                            (_,True) -> 96)
               ,ex [centerY, Ft.size 24,  Ft.color colorPalette.txSoft2] <|
                   if subPageInfo.autoRenew
                   then if subPageInfo.month3Package
                           then " / 3 Months"
                           else " / Month"
                   else ""]
       ,co [width <| maximum 800 fill, centerX, alignBottom, padding 32, spacing 48]
           [el ([centerX, paddingXY 16 8
                ,Ft.size 32
                ,Ft.color colorPalette.txSoft
                ,Bg.color colorPalette.bgMain
                ,Bdr.rounded 16
                ,mouseOver [Ft.color colorPalette.bgMain
                           ,Bg.color colorPalette.highlightBlue
                           ,Bdr.color colorPalette.highlightBlue]
                ,Evt.onClick <| HomePageMsg <| POSTSubscription subPageInfo]
                ++ buttonStyle
                ++ colorTransitionStyle) <|
            ex [centerX] "Confirm"
           ,paymentConditions model]
       ]




agreementMainBoxPage model homePageInfo back =
  let colorPalette = currentColorPalette model
      subSection letter els = pg [] <| (ex [Ft.bold] <| letter ++ ". ") :: els
  in mainBoxPageWrapper model <|
       co [width fill, height fill, spacing 24, clipX
          ,Ft.size 16, Ft.bold, Ft.color colorPalette.txSoft, noSelection]
          [mainBoxPageTitle model back "User Agreement"
          ,co [width fill, height fill, spacing 80, paddingEach {bottom = 32, left = 64, right = 64, top = 0}
              ,Ft.regular
              ,scrollbars, htmlClass "hide-scroll"]
              [informationSection model "Information"
                 [subSection "A"
                    [tx "This site uses "
                    ,blueLink model "https://en.wikipedia.org/wiki/HTTP_cookie" "cookies"
                    ,tx " when you're signed in that are used to authenticate your use of this website with your user account."
                    ]
                 ,subSection "B"
                     [tx "This site uses "
                     ,blueLink model "https://en.wikipedia.org/wiki/Web_storage#Features" "Local Storage"
                     ,tx " to hold non-sensitive data such as visual settings in order to speed up interactions with this site."
                     ]
                 ,subSection "C"
                    [tx "Your "
                    ,blueLink model "https://en.wikipedia.org/wiki/IP_address" "IP address"
                    ,tx " is recorded with your chat messages for a brief time period and is accessible to admins of the website."
                    ]
                 ,subSection "D"
                    [tx "This site does not exchange, sell, track, or otherwise make public any information related to user accounts that are not made public by the account holder."
                    ]
                 ]
              ,informationSection model "Accounts"
                 [subSection "A"
                    [tx "You can request, at anytime, all data stored that is related to your user account or removal of your user account by emailing "
                    ,styleEmail streamerInfo.emails.support
                    ,tx ". See Rights Reserved B."
                    ]
                 ,subSection "B"
                    [tx "Your private messages are not removed from those you have sent them to but your username will appear obfuscated."
                    ]
                 ,subSection "C"
                    [tx "Chat logs are kept seperately and must be seperately requested to be removed. You can request they be removed by contacting the Website's or Log Website's maintainer if they are maintained by the website. This request will be actioned upon within a reasonable timeframe."
                    ]
                 ]
              ,informationSection model "User Agreement 2"
                 [subSection "A"
                    [tx "You must not attempt to gain access to data, user accounts, or information that is not yours."
                    ]
                 ,subSection "B"
                    [tx "You must not create user profiles using automated code."
                    ]
                 ,subSection "C"
                    [tx "You must not create more than one user profile to access the site. If you have lost access to a user profile previously created, contact the Website's maintainer to have it deleted before creating a new account."
                    ]
                 ]
              ,informationSection model "Donations & Subscriptions"
                 [subSection "A"
                    [tx "Subscriptions are considered donations. Their use, along with any Subscription/Donation Messages that have yet to be used, depends on you maintaining access to your account and will not be accessible if banned."
                    ]
                 ,subSection "B"
                    [tx "Subscription/Donation Messages are not guaranteed to appear on screen and may not be read aloud depending on factors that can be outside of the streamer's control. Buying a Subscription may use or prevent another Subscription Message to be sent for up to 29 days even if left blank."
                    ]
                 ,subSection "C"
                   [tx "Randomly Gifted Subscriptions are considered donations. There may be no remaining users who can be Gifted a Subscription. Users currently or recently in chat are first pooled and randomly chosen followed by the total pool of registered accounts."
                   ]
                 ]
              ,informationSection model "Rights Reserved"
                 [subSection "A"
                    [tx "These and others rules on this website are subject to change at any time."
                    ]
                 ,subSection "B"
                    [tx "Actions regarding retreiving or removing account information will usually take uo to 30 days to process, but may take up to several months."
                    ]
                 ,subSection "C"
                    [tx "Your account can be deleted or blocked from accessing the site or restricted from using site features at any time."
                    ]
                 ,subSection "D"
                    [tx "Subscriptions and Donations may or may not be refunded at the sole descretion of the person subscribed or donated to."
                    ]
                 ,subSection "E"
                    [tx "Images, Graphics, Audio, Video Content, and other material on this website is the sole copyright of their respective creator(s) unless otherwise noted."
                    ]
                 ]
              ,elMaybe model.commonInfo.localInfo.zone <| \zone ->
                 let agreementUpdate = 1613258147
                     posix = Time.millisToPosix (1000 * agreementUpdate)
                     chars2 n = if String.length n == 1
                                   then "0" ++ n
                                   else n
                     time = (case Time.toMonth zone posix of
                               Time.Jan -> "Jan"
                               Time.Feb -> "Feb"
                               Time.Mar -> "Mar"
                               Time.Apr -> "Apr"
                               Time.May -> "May"
                               Time.Jun -> "Jun"
                               Time.Jul -> "Jul"
                               Time.Aug -> "Aug"
                               Time.Sep -> "Sep"
                               Time.Oct -> "Oct"
                               Time.Nov -> "Nov"
                               Time.Dec -> "Dec")
                        ++ " "
                        ++ String.fromInt (Time.toDay zone posix)
                        ++ ", "
                        ++ String.fromInt (Time.toYear zone posix)
                        ++ ", "
                        ++ chars2 (String.fromInt <| Time.toHour zone posix)
                        ++ ":"
                        ++ chars2 (String.fromInt <| Time.toMinute zone posix)
                 in ex [centerX, alignBottom] <| "This agreement was last updated: " ++ time
              ]
          ]




--------------------------------------------------------------------------------





subTierImage model tier = image [width <| px 180, height <| px 180, centerX] <|
  case Array.get (tier - 1) streamerInfo.subscriptionTiersLogos of
    Just subTier -> {src = subTier.image.url
                    ,description = "Tier" ++ String.fromInt tier ++ "SubImage"}
    _ -> {src = "", description = "NoSubImage"}


--------------------------------------------------------------------------------
-- Donation Conditions

subscriptionConditions =
  []

donationConditions model =
  []

-- elm ui boxes tx incorrctly atm, temporarily added a height constraint
-- to increase it by 2 pixels to fit the tx
paymentConditions model =
  let colorPalette = currentColorPalette model
  in co [width fill, height <| px 50, alignBottom, spacing 20
        ,Ft.size 14, Ft.color colorPalette.txSoft]
     <| [ro [centerX] <|
            userAgreementCondition model
        ,ro [centerX] <|
            paypalCondition model]

userAgreementCondition model =
  let colorPalette = currentColorPalette model
  in [tx "By continuing you are confirming you have read the "
     ,ex [Ft.color colorPalette.highlightBlue
        ,mouseOver [Ft.color colorPalette.highlightBlueBright]
        ,pointer
        ,Evt.onClick <| HomePageMsg <| AppendSubPage HomeSubPageAgreement]
        "user agreement"]

paypalCondition model =
  [tx "All payments use "
  ,blueLink model "https://www.paypal.com/" "PayPal"]
