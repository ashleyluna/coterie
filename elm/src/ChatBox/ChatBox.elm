module ChatBox.ChatBox exposing (..)

import Array
import Browser.Dom
import Dict exposing (Dict)
import Dict.Extra as DictE
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Maybe.Extra as MaybeE
import Result.Extra as ResultE
import Task
import Time exposing (Posix, Zone)
import Tuple

import Svg
import Svg.Attributes as SvgA

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La

import ChatBox.ChatRoom exposing (..)
import ChatBox.MessageBox exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)
import Update.LiveInfo exposing (..)
import Update.CommonInfo exposing (..)








chatBox : Model -> ChatLocale -> String -> Chat -> ChatBox -> Element ChatBoxMsg
chatBox model chatLocale containerName chat cBox =
  let colorPalette = currentColorPalette model
      chatBoxName = containerName ++ "chatbox-"
  in co [width fill, height fill, htmlID "chatBox"] <|
        if not model.setUp.profile
        then [el [width fill, height fill
                 ,Bg.color colorPalette.bgMain]
                 Element.none]
        else [chatBoxHeader model chat cBox
             ,case model.commonInfo.profile of
                Just profile -> el
                  ([width fill, height fill]
                   ++ let wrapper f = List.singleton <| inFront <| f model cBox
                      in case cBox.chatBoxOverlay of
                           ShoutOutBoxOverlay -> wrapper <| shoutOutBoxOverlay chatBoxName
                           PointsRewardsOverlay -> wrapper <| pointsRewardsOverlay chatBoxName
                           WhispersOverlay info -> wrapper <| whispersOverlay chatBoxName info profile
                           SettingsOverlay info -> wrapper <| settingsOverlay chatBoxName info profile
                           CommunityOverlay info -> wrapper <| communityOverlay chatBoxName info chat.users
                           _ -> []) <|
                  Element.map (Msg << ChatRoomMsg) <|
                    chatRoom model chatLocale chatBoxName chat cBox.chatRoom
                _ -> el
                  ([width fill, height fill, scrollbars]
                   ++ let wrapper f = List.singleton <| inFront <| f model cBox
                      in case cBox.chatBoxOverlay of
                           RegisterOverlay info -> wrapper <| registerOverlay info
                           _ -> []) <|
                  Element.map (Msg << ChatRoomMsg << Msg << MessageBoxMsg) <|
                    messageBox model False chatBoxName chat cBox.chatRoom.messageBox]




chatBoxHeader : Model -> Chat -> ChatBox -> Element ChatBoxMsg
chatBoxHeader model chat cBox =
  let colorPalette = currentColorPalette model
      commonInfo = model.commonInfo
      overlayButton name overlay =
        el [width <| fillPortion 1] <<
        -- padding is for small buffer, in case user barely misclicks button
        el [centerX, paddingXY 8 0, Ft.size 24
           ,Evt.onClick <| Msg <| SetChatBoxOverlay <| overlay] <<
        el [hoverTitle name] <<
        bubbleActiveMainFA model (isChatBoxOverlayOn cBox overlay) <<
        faIcon [padding 8]
  in case commonInfo.profile of
       Nothing ->
         let initialRegisterOverlayInfo = RegisterOverlay
               {remember = False
               ,signUp = True
               ,username = ""
               ,invalidUsername = 0}
         in el [width fill, centerY, paddingXY 0 8
               ,Bg.color colorPalette.bgMain] <|
            el [centerX] <|
            el [width <| fillPortion 1
               ,Evt.onClick <| Msg <| SetChatBoxOverlay initialRegisterOverlayInfo] <|
            el [centerX, Ft.size 18] <|
            bubbleActiveMain model (isChatBoxOverlayOn cBox initialRegisterOverlayInfo) "Sign In"
       Just profile ->
         let pronouns = Maybe.withDefault "" <| Maybe.andThen .pronouns commonInfo.profile
             defaultNameColor = defaultChromaColor <| case profile.nameColor.defaultNameColor of
                   Err c -> c
                   Ok c -> c
             initialWhispersOverlay = WhispersOverlay <|
               {userSearch = ""}
             initialSettingsOverlaySettings = SettingsOverlay <|
               {headerPosition = 1
               ,pronouns = pronouns
               ,invalidPronouns = 0
               ,defaultNameColor = Result.toMaybe profile.nameColor.defaultNameColor
               ,left = Maybe.withDefault defaultNameColor profile.nameColor.left
               ,right = Maybe.withDefault defaultNameColor <| MaybeE.or profile.nameColor.right profile.nameColor.left
               ,mode = profile.nameColor.mode
               }
             initialCommunityOverlay = CommunityOverlay <|
               {userSearch = ""}
         in el [width fill, height <| px 54, alignTop, spacing 20
               ,Ft.bold
               ,Bg.color colorPalette.bgMain
               --,Bdr.color colorPalette.bgMain2, Bdr.widthXY 0 2
               ,noSelection] <|
               --[arrowButton MainBoxHeaderPosition "M25 12 l-12 13 l12 13"
               --,--if model.mainBoxHeaderPosition
               --   then ro [width fill, centerY, spaceEvenly, paddingXY 0 8, Ft.size 18] <|
               --           [
               --           --overlayButton MutualAidOverlay <| "Mutual Aid"
               --           --,overlayButton initialDonateOverlaySettings <| "Donate"
               --           ----,overlayButton initialSubscribeOverlaySettings <| "Subscribe"
               --           --,overlayButton initialSupportOverlaySettings <| "Support"
               --           ]
               --   else
            el [width fill, paddingXY 0 8] <|
            ro [width fill, height <| px 34, centerY, spaceEvenly]
               [
               --overlayButton ShoutOutBoxOverlay <| "fas fa-bread-slice"
               --,overlayButton PointsRewardsOverlay <| "Pt"
               --,
               overlayButton "Whispers" initialWhispersOverlay <| "fas fa-comments"
               ,overlayButton "Settings" initialSettingsOverlaySettings <| "fas fa-cog"
               ,overlayButton "Community" initialCommunityOverlay <| "fas fa-users"
               ]
            --,arrowButton MainBoxHeaderPosition "M15 12 l12 13 l-12 13"
            --]


shoutOutBoxOverlay name model cBox =
  let colorPalette = currentColorPalette model
  in --mainBoxOverlayWrapper model <|
       column [width fill, height fill, paddingXY 0 16]
              [titleHeaderMiddle model "Bread Box"
              ]



pointsRewardsOverlay name model cBox =
  let colorPalette = currentColorPalette model
  in --mainBoxOverlayWrapper model <|
       column [width fill, height fill, paddingXY 0 16]
              [titleHeaderMiddle model "Points Rewards"
              ]



whispersOverlay : String -> WhispersOverlayRecord -> ProfileRecord -> Model -> ChatBox -> Element ChatBoxMsg
whispersOverlay chatBoxName info profile model cBox =
  let colorPalette = currentColorPalette model
  in chatBoxOverlayWrapper model "Whispers"
      [el [width shrink, centerX, paddingXY 0 40] <|
       In.text (fieldStyle model)
               {onChange = \str -> Msg <| UpdateChatBoxOverlay <| WhispersOverlay {info | userSearch = str}
               ,text = info.userSearch
               ,placeholder = placeholder (Ft.color colorPalette.txSoft2) "Search User"
               ,label = In.labelHidden "userSearchWhisper"}
      ,if model.liveInfo.whispers == Nothing
          then ex
            [centerX, centerY
            ,Ft.size 24, Ft.color colorPalette.txSoft2]
            "No Messages"
          else elmUIVBar model cBox.elmBar (Msg << ChatBoxElmBarMsg) (chatBoxName ++ "chatbox-whispers-overlay")
                         (\viewport elmBarMsg -> BatchMsgs <|
                           [Msg <| ChatBoxElmBarMsg elmBarMsg]
                           --++
                         ) <|
           em []
      ]




settingsOverlay : String -> SettingsOverlayRecord -> ProfileRecord -> Model -> ChatBox -> Element ChatBoxMsg
settingsOverlay chatBoxName info profile model cBox =
  let colorPalette = currentColorPalette model
      --authConnButton top bottom left right social wrapper = el [centerX] <|
      --  (if not info.signUp || info.invalidUsername <= 1 then wrapper else el []) <|
      --  flip faIcon social <|
      --    [paddingEach {top = top, bottom = bottom, left = left, right = right}
      --    ,Ft.size 48
      --    ,Bg.color colorPalette.bgMain
      --    ,Bdr.rounded 20
      --    ,noSelection
      --    ]
      --    ++ colorTransitionStyle
      --    ++ if not info.signUp || info.invalidUsername == 1
      --          then [Ft.color colorPalette.txSoft
      --               ,mouseOver [Ft.color colorPalette.bgMain
      --                          ,Bg.color colorPalette.mainHighlight]
      --               ,pointer]
      --          else [Ft.color colorPalette.txSoft2]
  in chatBoxOverlayWrapper model "Settings"
        [el [width fill, paddingXY 0 32] <|
         slidingHeader model info.headerPosition
           cBox.chatBoxSlideDirection
           cBox.initiateChatBoxOverlayHeaderSliding
           (\int -> SettingsOverlay {info | headerPosition = int})
           (\b1 b2 o -> Msg <| SlideChatBoxOverlayHeader b1 b2 o)
           ["General","Account","Identity"]
           -- General theme, pronouns, badges, name color
           -- Chat emphasis, text sizem show, animate, nsfw, etc
           -- Account username, auth,
        ,--elmUIVBar model ChatBoxElmBarMsg (chatBoxName ++ "chatbox-settings-overlay") cBox.elmBar <|
         co [width fill, height fill, paddingXY 16 0, spacing 48
            ,Ft.size 18, Ft.bold, scrollbars] <|
         case info.headerPosition of
           -- General
           1 ->
             [-- Theme Mode
              -- Messages
                -- Show Time Stamps
                -- Show Emotes
                -- Animate Emote
                -- Show Badges
                -- Style Usernames
                -- Text Emphasis
                -- Text Size
              -- Filters
                -- Mention Ingorned Users
                -- Curse Swear Words
                -- NSFW
              -- Other
                -- Allow Forced Stream/Chat Refreshes
                -- Auto Complete Helper
             ]
           -- Account
           2 ->
             [-- Username
              -- Auth Connections
              co [width fill, spacing 20]
                 [el [width fill, centerX, paddingBottom 20, Ft.color colorPalette.txSoft] <|
                     sectionTitle model "Auth Connections"
                 --,
                 ]
             ,el [centerX, Ft.color colorPalette.txSoft
                 ,Evt.onClick <| GlobalMsg <| Msg GETLogOut] <|
              bubbleMain model
                 "Log Out"
             ]
           -- Identity
           _ ->
             [-- Pronouns
              co [width fill, spacing 20]
                 [el [width fill, centerX, paddingBottom 20, Ft.color colorPalette.txSoft] <|
                     sectionTitle model "Pronouns"
                 ,el [width shrink, centerX] <|
                  fieldCheck model In.text
                      info.invalidPronouns
                      (\str -> Msg <| UpdateChatBoxOverlay <| SettingsOverlay {info | pronouns = str})
                      (\str -> Msg <| PronounsCheck str)
                      info.pronouns
                      "ex: Them/Them"
                      "pronounsInput"
                 ,if (not <| info.invalidPronouns >= 2) then em [height <| px 16]
                     else fieldErrorText model <| case info.invalidPronouns of
                       2 -> "Character length must be 3-24. Characters must be A-Z or '/'" -- TODO allow '/'
                       _ -> "Pronouns must not include profanity"
                 ,el ([centerX, paddingBottom 16, Ft.size 20]
                     ++ listIf (info.invalidPronouns == 1)
                          [Evt.onClick <| Msg <| POSTSetPronouns info.pronouns]) <|
                     bubbleMain model "Save Pronouns"
                 ]
              -- Badges
             ,badgesSelection model profile info
              -- Name Color
             ,nameColorSelection model profile info
             ]
        --,
        ]


badgesSelection : Model -> ProfileRecord -> SettingsOverlayRecord -> Element ChatBoxMsg
badgesSelection model profile info =
  let colorPalette = currentColorPalette model
      staticInfo = model.commonInfo.staticInfo
      settings = model.commonInfo.settings
      firstBadge = profile.badges.firstBadge
      secondBadge = profile.badges.secondBadge
      seasonName int = "Season " ++ String.fromInt int
      getBadge str = Dict.get str <| Dict.union staticInfo.badges
        (Dict.fromList <| List.indexedMap (pair << seasonName << (+) 1) staticInfo.seasonBadges)
      collection = profile.badges.collection
      isInUse str = firstBadge == Just str || secondBadge == Just str
      badgeDisplayWrapper addOrRemove doHighlight evt = el -- name is for hover
        ([width <| px 48, height <| px 48, circleStyle, Bg.color colorPalette.bgMain3]
        ++ colorTransitionStyle
        ++ listIf doHighlight
           [Evt.onClick evt
           ,mouseOver [Bg.color <| if addOrRemove
             then colorPalette.mainHighlight
             else colorPalette.highlightRed]])
      badgeDisplay int maybeBadge = badgeDisplayWrapper False (not <| maybeBadge == Nothing)
        (Msg <| POSTUnEquipBadge int) <|
        case maybeBadge of
          Nothing -> Element.none
          Just emote -> image
            ([width <| px 32, height <| px 32, centerX, centerY
             ,hoverTitle "Unequip"])
            {src = emote.image.url, description = emote.name}
      badgeSelector inUse allow name emote = badgeDisplayWrapper True
        (allow && not inUse && not (firstBadge /= Nothing && secondBadge /= Nothing))
        (Msg <| POSTEquipBadge name <| if firstBadge == Nothing then 1 else 2) <|
        elIf (not inUse) <|
          image ([width <| px 32, height <| px 32, centerX, centerY
                 ,hoverTitle name]
                 ++ listIf (not allow) [htmlStyle "filter" "grayscale(100%)"])
                {src = emote.image.url, description = emote.name}
  in co [width fill, spacing 20] <|
     [el [width fill, centerX, paddingBottom 20, Ft.color colorPalette.txSoft] <|
         sectionTitle model "Badges"
     ,ex [centerX, Ft.size 16, Ft.color colorPalette.txSoft2]
         "Equipped Badges"
     ,ro [centerX, spacing 40] <|
         flip List.indexedMap [firstBadge,secondBadge] <| \int maybeBadge ->
              badgeDisplay (int + 1) <| Maybe.andThen getBadge maybeBadge
     ,ex [centerX, paddingTop 20, Ft.size 16, Ft.color colorPalette.txSoft2]
         "All Badges"
     ,wr [centerX, spacing 20] <| List.concat
         [flip List.indexedMap staticInfo.seasonBadges <| \int emote ->
            badgeSelector (isInUse <| seasonName <| int + 1)
                          (profile.account.season <= int + 1) (seasonName <| int + 1) emote
         ,Dict.values <|
            flip Dict.map staticInfo.badges <| \str emote ->
            badgeSelector (isInUse str)
                          (List.member str collection) str emote
         ]
     ]




nameColorSelection : Model -> ProfileRecord -> SettingsOverlayRecord -> Element ChatBoxMsg
nameColorSelection model profile info =
  let colorPalette = currentColorPalette model
      settings = model.commonInfo.settings
      left = info.left
      right = info.right
      chromaCircle size themeMode chromaColor = chromaBlock themeMode chromaColor <|
        [HtmlA.style "width" <| String.fromInt size ++ "px"
        ,HtmlA.style "height" <| String.fromInt size ++ "px"
        ,HtmlA.style "border-radius" "50%"]
      defaultColorSelectorAttrs selectedDefault =
        [padding 4, circleStyle
        ,Bg.color <| if selectedDefault == info.defaultNameColor
            then colorPalette.mainHighlight else colorPalette.bgMain]
      defaultColorSelector defaultColorName = el
        (defaultColorSelectorAttrs <| Just defaultColorName)
        <| el (if Just defaultColorName == info.defaultNameColor then []
                  else [pointer, Evt.onClick <| Msg <| POSTSetDefaultColor <| Just defaultColorName])
        <| chromaCircle 28 settings.themeMode
        <| defaultChromaColor defaultColorName
      randomColorSelector = el
        (defaultColorSelectorAttrs Nothing)
         <| flip el Element.none <|
              [width <| px 28, height <| px 28, circleStyle
              ,htmlStyle "background" <| "linear-gradient(to bottom, lime 33%, red 33% 66%, blue 66% 100%)"
              ] ++
              if Nothing == info.defaultNameColor then []
                 else [pointer, Evt.onClick <| Msg <| POSTSetDefaultColor Nothing]
      hueSlider align value onChange =flip el Element.none <|
        [width <| px 370, height <| px 20 -- 306
        ,Bdr.rounded 8
        ,htmlStyle "background" <| "linear-gradient(to right,"
                                ++ "rgb(100% 26.43% 58.18%),"
                                ++ "rgb(100% 30.88% 25.03%),"
                                ++ "rgb(77.2% 50.56% 0%),"
                                ++ "rgb(51.85% 60.16% 0%),"
                                ++ "rgb(0% 65.56% 26.8%),"
                                ++ "rgb(0% 64.14% 56.08%),"
                                ++ "rgb(0% 62.83% 71.45%),"
                                ++ "rgb(0% 60.78% 88.75%),"
                                ++ "rgb(48.22% 53.34% 100%),"
                                ++ "rgb(94.13% 21.28% 100%),"
                                ++ "rgb(100% 26.43% 58.18%))"
        ,inFront <| In.slider [width <| px 370, height <| px 20]
           {min = 0, max = 360, value = toFloat value, step = Just 1
           ,label = In.labelHidden "Hue Slider"
           ,onChange = Msg << UpdateChatBoxOverlay << SettingsOverlay << onChange << round
           ,thumb = In.thumb [width <| px 8, height <| px 20
                             ,Bdr.rounded 8, Bdr.width 2, Bdr.color <| rgb 0 0 0
                             ,Bg.color <| .bgMain <| currentColorPalette_ True
                             ,turnOffElmUIBlueFocus]}]
      vcSlider themeMode vOrC label value onChange = ro
         [height <| px 16]
         [In.slider
            [width <| px 140, height <| px 4, Bdr.rounded 4
            ,Bg.color colorPalette.txSoft2]
            {min = if vOrC && not themeMode then 25 else 0
            ,max = if vOrC then if themeMode then 90 else 100 else 130
            ,value = toFloat value, step = Just 1
            ,label = In.labelHidden <| String.append label " Slider"
            ,onChange = Msg << UpdateChatBoxOverlay << SettingsOverlay << onChange << round
            ,thumb = In.thumb [width <| px 16, height <| px 16
                              ,Bdr.rounded 8
                              ,turnOffElmUIBlueFocus
                              ,Bg.color colorPalette.txSoft2
                              ,above <| el [centerX, Ft.size 18
                              ,Ft.color colorPalette.txSoft]
                                     <| tx <| String.fromInt value]}]
      chromaValuePickers modeName themeMode sliders1 sliders2 = co
       [width fill, spacing 40]
       [co [width fill, spacing 20]
           [el [centerX, Ft.size 18, Ft.color colorPalette.txSoft] <| tx modeName
           ,ro [centerX, padding 10, spacing 12, Bdr.rounded 40, Bdr.color colorPalette.txSoft
               ,Bg.color <| .bgMain <| currentColorPalette_ themeMode]
               [el [] <| chromaCircle 40 themeMode left
               ,el [Ft.size 24] <| mkChromaUsername themeMode profile.username <|
                ChromaNameGradient {left = left, right = right, mode = info.mode}
               ,el [] <| chromaCircle 40 themeMode right]]
       ,ro [centerX, spacing 16] <|
           let showSliders int = co [spacing 20, alpha <| if nameColorLevel profile >= int then 1 else 0.7]
           in [showSliders 1 sliders1
              ,el [Ft.size 16, Ft.color colorPalette.txSoft, moveUp 4] <|
                  showSliders 1 [ex [centerX] "Value"
                                ,ex [centerX] "Chroma"]
              ,showSliders 2 sliders2]]
      chromaModeSelector mode = el
        (if mode == info.mode then []
            else [Evt.onClick <| Msg <| OverSettingsOverlay <| \info_ ->
                    {info_ | mode = mode}]) <|
        bubbleActiveMain model (info.mode == mode) <|
        showChromaMode mode
  in co [width fill, spacing 40]
        [co [width fill, centerX, spacing 12]
          [el [width fill, centerX, paddingBottom 20, Ft.color colorPalette.txSoft] <|
              sectionTitle model "Name Color"
          ,ex [centerX, Ft.size 16, Ft.color colorPalette.txSoft3]
              "How Your Username Will Appear In Chat"
          ,el [centerX, Ft.size 28] <|
              mkChromaUsername settings.themeMode profile.username <| profileNameColorAppearance profile]
        ,co [centerX, spacing 24, paddingTop 16]
            [ex [centerX, Ft.size 18, Ft.color colorPalette.txSoft]
                "Default Color"
            ,wrappedRow [width <| px 260, centerX, spacing 8] <|
              (List.map defaultColorSelector
                [Blue, Azure, Sky
                ,Turquoise, Turtle, Green, Slime
                ,Yellow, Bronze, Mocha
                ,Orange, DarkRed, Red, Rose
                ,Pink, Purple, Lavender
                ])
              ++ [randomColorSelector]]
        ,case nameColorLevel profile of
          2 -> Element.none
          1 -> pg [width <| px 300, centerX, Ft.center, Ft.color colorPalette.highlightRed] <|
                  [tx "You may set a gradient, but the left side color will be used for both sides in chat"]
          _ -> pg [width <| px 320, centerX, Ft.center, Ft.color colorPalette.highlightRed] <|
                  [tx "You may set a custom name color, but it will not appear in chat"]
        ,co [centerX, spacing 24]
            [ex [centerX, Ft.size 22, Ft.color colorPalette.txSoft] "Gradient"
            ,co [centerX, spacing 48]
                [chromaValuePickers "Light Mode" True
                   [vcSlider True  True  "Light Value"  left.valueLight   <| \int -> {info | left  = {left | valueLight   = int}}
                   ,vcSlider True  False "Light Chroma" left.chromaLight  <| \int -> {info | left  = {left | chromaLight  = int}}]
                   [vcSlider True  True  "Light Value"  right.valueLight  <| \int -> {info | right = {right | valueLight  = int}}
                   ,vcSlider True  False "Light Chroma" right.chromaLight <| \int -> {info | right = {right | chromaLight = int}}]
                ,chromaValuePickers "Dark Mode" False
                   [vcSlider False True  "Dark Value"  left.valueDark   <| \int -> {info | left  = {left  | valueDark  = int}}
                   ,vcSlider False False "Dark Chroma" left.chromaDark  <| \int -> {info | left  = {left  | chromaDark = int}}]
                   [vcSlider False True  "Dark Value"  right.valueDark  <| \int -> {info | right = {right | valueDark  = int}}
                   ,vcSlider False False "Dark Chroma" right.chromaDark <| \int -> {info | right = {right | chromaDark = int}}]
                ,co [centerX, spacing 24]
                    [hueSlider alignLeft left.hue <| \int -> {info | left = {left | hue = int}}
                    ,hueSlider alignRight right.hue <| \int -> {info | right = {right | hue = int}}]]]
        ,co [centerX, spacing 20]
            [ex [centerX, Ft.size 18, Ft.color colorPalette.txSoft] "Mode"
            ,co [width <| px 260, centerX, spacing 16, Ft.center] <|
                List.map (ro [centerX, spacing 8] << List.map chromaModeSelector) <|
                [[LRGB, RGB, LCH, LAB]
                ,[HSL, HSV, HSI]]]
        ,el [centerX, paddingBottom 16
            ,Ft.size 22, Evt.onClick <| Msg <| POSTSetNameColor left right info.mode] <|
            bubbleMain model "Save Gradient"
        ]




communityOverlay : String -> CommunityOverlayRecord -> ChatUserList -> Model -> ChatBox -> Element ChatBoxMsg
communityOverlay chatBoxName info users model cBox =
  let colorPalette = currentColorPalette model
      filterUsers = Dict.filter <| \name _ -> info.userSearch == ""
                                           || String.contains info.userSearch (String.toLower name)
      simpleDict f = Dict.toList << Dict.map (\_ -> f)
      specialUsers : List (String, List (String, NameColor))
      specialUsers = List.filter (\(_, ls) -> ls /= []) <|
        List.concatMap (simpleDict <| simpleDict .nameColor << filterUsers) users.specialUsers
      chatters : List (String, NameColor)
      chatters =
        --List.take (50 * MaybeE.unwrap 1 .stack cBox.elmBar.infiniteScroll) <|
        simpleDict .nameColor <| filterUsers users.chatters

      usernameStyle username nameColor = el
         [centerX] <|
         mkChromaUsername model.commonInfo.settings.themeMode
                          username nameColor
      roleGroupStyle roleName userList = co
        [width fill, spacing 20, Ft.color colorPalette.txSoft]
        [ex [centerX, Ft.size 20, Ft.bold, Ft.color colorPalette.txSoft]
            roleName
        ,userList
        ]
  in chatBoxOverlayWrapper model "Community" <|
       [el [width shrink, centerX, paddingTop 40] <|
        In.text (fieldStyle model)
                {onChange = \str -> Msg <| UpdateChatBoxOverlay <| CommunityOverlay {info | userSearch = str}
                ,text = info.userSearch
                ,placeholder = placeholder (Ft.color colorPalette.txSoft2) "Filter Users"
                ,label = In.labelHidden "userSearchCommunity"}
       ,elmUIVBar model cBox.elmBar (Msg << ChatBoxElmBarMsg)
         (chatBoxName ++ "chatbox-community-overlay")
         (\viewport elmBarMsg -> BatchMsgs <|
           [Msg <| ChatBoxElmBarMsg elmBarMsg]
           --++
         ) <|
         if specialUsers == [] && chatters == []
            then ex [centerX, centerY
                    ,Ft.bold, Ft.size 20, Ft.color colorPalette.txSoft]
                    "No Users"
            else co [width fill, spacing 40, paddingXY 16 40] <|
                    flip List.map specialUsers (\(roleName, userList) -> roleGroupStyle roleName <|
                      co [width fill, spacing 8, Ft.size 16] <|
                         flip List.map userList <| \(username, nameColor) ->
                          usernameStyle username nameColor)
                    ++ listIf (chatters /= [])
                         [roleGroupStyle streamerInfo.defaultRole <| co
                           [width fill, spacing 8, Ft.size 16] <|
                           flip List.map chatters <| \(username, nameColor) ->
                             usernameStyle username nameColor
                         ]
       ]



registerOverlay : RegisterRecord -> Model -> ChatBox -> Element ChatBoxMsg
registerOverlay info model cBox =
  let colorPalette = currentColorPalette model
      updateRegister = Msg << UpdateChatBoxOverlay << RegisterOverlay
      socialButton top bottom left right social wrapper = el [centerX] <|
        (if not info.signUp || info.invalidUsername <= 1 then wrapper else el []) <|
        flip faIcon social <|
          [paddingEach {top = top, bottom = bottom, left = left, right = right}
          ,Ft.size 48
          ,Bg.color colorPalette.bgMain
          ,Bdr.rounded 20
          ,noSelection
          ]
          ++ colorTransitionStyle
          ++ if not info.signUp || info.invalidUsername == 1
                then [Ft.color colorPalette.txSoft
                     ,mouseOver [Ft.color colorPalette.bgMain
                                ,Bg.color colorPalette.mainHighlight]
                     ,pointer]
                else [Ft.color colorPalette.txSoft2]
  in chatBoxOverlayWrapper model (if info.signUp then "Sign Up" else "Log In")
       [co [width <| maximum 420 fill, centerX, spacing 40, paddingTop 40
           ,Ft.size 24] <| List.map (ro [width fill]
                        << List.map (el [width <| fillPortion 1]))
           [[socialButton 10 4 8 6 "fab fa-twitch" <|
               mkLink_ [if not info.signUp || info.invalidUsername == 1
                           then pointer
                           else defaultCursor] <|
                       "https://id.twitch.tv/oauth2/authorize?client_id=" ++ streamerInfo.secrets.twitchClientId ++ "&redirect_uri=" ++ apiUrl model.commonInfo.localInfo.url ++ "register/twitch&response_type=code&scope=user:read:email%20user:read:subscriptions"
            ,socialButton 7 7 8 6 "fab fa-google" <|
               el [centerX, Evt.onClick <| Msg StartGoogleSignIn]
            ]
           ,[socialButton 10 6 10 10 "fab fa-discord" <|
               el []
            ,socialButton 6 6 6 6 "fab fa-reddit" <|
               el []
            ]
           ,[socialButton 7 5 7 7 "fab fa-twitter" <|
               el []
            ]
           ]
       ,ro [centerX, paddingXY 0 50, spacing 14]
           [ex [Ft.size 20, Ft.color colorPalette.txSoft]
               "Rememeber Me"
           ,buttonSlider model info.remember <|
               updateRegister {info | remember = not info.remember}
           ]
       ,el [centerX, paddingBottom 40
           ,Ft.color colorPalette.txSoft, Ft.size 14
           ,Evt.onClick <| updateRegister {info | signUp = not info.signUp}
           ] <|
           bubbleMain model <| if info.signUp then "Log In Instead" else "Sign Up Instead"
       ,el [width <| maximum 420 fill, height <| px 144, centerX, clip
           ,paddingTop 20] <|
           co [width fill, height <| px 100, spacing 20
              ,htmlStyle "bottom" <| if info.signUp then "0px" else "124px"
              ,htmlStyle "transition" "bottom 1s ease-in-out"] <|
              [el [width fill, centerX, Ft.size 20] <|
                  fieldCheck model In.text
                    info.invalidUsername
                    (\str -> updateRegister {info | username = str})
                    (\str -> Msg <| SignUpCheck str)
                    info.username
                    "Username"
                    "usernameInput"
              ,elIf (info.invalidUsername >= 2) <|
                    fieldErrorText model <| case info.invalidUsername of
                      2 -> "Character length must be 3-20. Characters must be A-Z or 0-9" --TODO allow underscores
                      3 -> "Usernames must not include profanity"
                      _ -> "This username is already in use"
              ]
       ,co [centerX, spacing 8, alignBottom, paddingBottom 8, Ft.size 16]
           [ex [centerX, Ft.color colorPalette.txSoft]
               "By continuing you are confirming you have read the "
           ,ex [centerX, Ft.color colorPalette.highlightBlue
               ,mouseOver [Ft.color colorPalette.highlightBlueBright]
               ,pointer
               ,Evt.onClick <| GlobalMsg <| Msg <| HomePageMsg <| AppendSubPage HomeSubPageAgreement]
               "user agreement"]
       ]




--------------------------------------------------------------------------------


chatBoxOverlayWrapper : Model -> String -> List (Element ChatBoxMsg) -> Element ChatBoxMsg
chatBoxOverlayWrapper = overlayWrapper <| Msg <| SetChatBoxOverlay NoChatBoxOverlay
