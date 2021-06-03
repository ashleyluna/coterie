module Internal.Style exposing (..)

import Browser.Dom exposing (Viewport)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JD
import Time

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La

import Accessors exposing (..)
import List.Extra

import Internal.Internal exposing (..)
import Internal.Streamer exposing (..)

import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)


{-
-- element attribute order

el [width, height, centerX, centerY, align, spacing, padding
   ,Bg.color
   ,Bdr.width, Bdr.rounded, Bdr.color
   ,Ft.cente, Ft.size, Ft.color]

-}

-- Short Hand
em = flip el Element.none
co = column
ro = row
tx = text
ex attrs = el attrs << text
pg = paragraph
pt attrs str = pg attrs [tx str]
wr = wrappedRow




-- Style Pieces
paddingTop int = paddingEach {top = int, bottom = 0, left = 0, right = 0}
paddingBottom int = paddingEach {bottom = int, top = 0, left = 0, right = 0}
paddingLeft int = paddingEach {left = int, top = 0, bottom = 0, right = 0}
paddingRight int = paddingEach {right = int, top = 0, bottom = 0, left = 0}


htmlStyle str1 str2 = Element.htmlAttribute (HtmlA.style str1 str2)
htmlAttr str1 str2 = Element.htmlAttribute (HtmlA.attribute str1 str2)
htmlID = Element.htmlAttribute << HtmlA.id
htmlClass = Element.htmlAttribute << HtmlA.class
hoverTitle = Element.htmlAttribute << HtmlA.title


defaultCursor = htmlStyle "cursor" "default"

noSelection = htmlStyle "user-select" "none"

circleStyle = htmlStyle "border-radius" "50%"

-- Attribute Pieces


--------------------------------------------------------------------------------

currentColorPalette : Model -> ColorPalette
currentColorPalette model =
  if model.commonInfo.settings.themeMode
     then streamerInfo.colors.lightMode
     else streamerInfo.colors.darkMode

currentColorPalette_ : Bool -> ColorPalette
currentColorPalette_ themeMode =
  if themeMode
     then streamerInfo.colors.lightMode
     else streamerInfo.colors.darkMode

--------------------------------------------------------------------------------
-- conditionals

elIf b e = if b then e else Element.none

elMaybe m f = case m of
  Just a -> f a
  _ -> Element.none

--------------------------------------------------------------------------------


finishedLayout model =
  let colorPalette = currentColorPalette model
  in layout
     [width fill, height fill
     ,Ft.family [Ft.typeface "Lato"
                ,Ft.typeface "Open Sans"
                ,Ft.typeface "Helvetica"
                ,Ft.typeface "Verdana"
                ,Ft.typeface "Roboto"
                ,Ft.sansSerif]
     --,Bg.color colorPalette.txSoft
     ]



mkLink attrs url elem = Element.newTabLink attrs {url = url, label = elem}
mkLink_ attrs url elem = Element.link attrs {url = url, label = elem}

faIcon attrs name = el attrs <| Element.html <|
  Html.i [HtmlA.class name]
         []

blueLink model url =
  let colorPalette = currentColorPalette model
  in tx >> mkLink
  [Ft.color colorPalette.highlightBlue
  ,mouseOver [Ft.color colorPalette.highlightBlueBright]
  ,pointer]
  url

mkTimeStamp mZone int =
  let time = Time.millisToPosix <| round <| toFloat int / 1000
      zone = case mZone of
                  Just z -> z
                  _ -> Time.utc
      hour = String.fromInt <| Time.toHour zone time
      minute = String.fromInt <| Time.toMinute zone time
  in hour ++ ":" ++
     if String.length minute == 1
        then "0" ++ minute
        else minute


dividerV model =
  let colorPalette = currentColorPalette model
  in em [width <| px 4, height fill
        ,Bg.color colorPalette.bgMain2]
dividerH model =
  let colorPalette = currentColorPalette model
  in em [width fill, height <| px 4
        ,Bg.color colorPalette.bgMain2]



--------------------------------------------------------------------------------



buttonSlider model bool msg =
  let colorPalette = currentColorPalette model
  in el ([width <| px 40, height <| px 20
             ,Bdr.rounded 20
             ,Evt.onClick msg
             ,Bg.color <| if bool then colorPalette.mainHighlight else colorPalette.bgMainVeryDark]
             ++ colorTransitionStyle) <|
     em [width <| px 20, height <| px 20
        ,Bdr.rounded 20
        ,Bg.color colorPalette.txSoft
        ,moveRight <| if bool then 24 else 0
        ,htmlStyle "transition" "transform 0.2s ease-in-out"
        ,htmlStyle "-webkit-transition" "transform 0.2s ease-in-out"]




--------------------------------------------------------------------------------
-- Bubbles

bubbleMain model str =
  let colorPalette = currentColorPalette model
  in bubbleSimple model
       {ft = colorPalette.txSoft
       ,bg = colorPalette.bgMain
       ,highlight = colorPalette.mainHighlight} <|
     el [paddingXY 6 8] <|
     tx str

bubbleActiveMain model active str =
  let colorPalette = currentColorPalette model
  in bubbleActive model active
       {ft = colorPalette.txSoft
       ,bg = colorPalette.bgMain
       ,activeBg = colorPalette.highlightBlue
       ,highlight = colorPalette.mainHighlight
       ,activeHighlight = colorPalette.highlightBlueBright} <|
     el [paddingXY 6 8] <|
     tx str

bubbleActiveMainFA model active =
  let colorPalette = currentColorPalette model
  in bubbleActive model active
       {ft = colorPalette.txSoft
       ,bg = colorPalette.bgMain
       ,activeBg = colorPalette.highlightBlue
       ,highlight = colorPalette.mainHighlight
       ,activeHighlight = colorPalette.highlightBlueBright}



-- highlight when hovered
bubbleSimple model colors = el <|
  [Bdr.rounded 12
  ,Ft.color colors.ft, Ft.bold
  ,Bg.color colors.bg
  ,mouseOver [Ft.color colors.bg
             ,Bg.color colors.highlight]
  ]
  ++ buttonStyle
  ++ colorTransitionStyle

-- + if bool highlight2
bubbleActive model active colors = el <|
  [Bdr.rounded 12
  ,Ft.bold
  ,mouseOver <| [Ft.color colors.bg
                ,Bg.color <| if active
                    then colors.activeHighlight
                    else colors.highlight]
  ]
  ++ buttonStyle
  ++ colorTransitionStyle
  ++ if active
        then [Ft.color colors.bg
             ,Bg.color colors.activeBg]
        else [Ft.color colors.ft
             ,Bg.color colors.bg]






--------------------------------------------------------------------------------
-- Chroma

chromaUsername themeMode username nameColor = Element.html <| case nameColor of
  ChromaNameGradient gradient -> Html.node "chroma-name-gradient"
    [HtmlA.attribute "username" username
    ,HtmlA.attribute "left-hue" <| String.fromInt gradient.left.hue
    ,HtmlA.attribute "left-chroma" <| getChroma themeMode gradient.left
    ,HtmlA.attribute "left-value" <| getValue themeMode gradient.left
    ,HtmlA.attribute "right-hue" <| String.fromInt gradient.right.hue
    ,HtmlA.attribute "right-chroma" <| getChroma themeMode gradient.right
    ,HtmlA.attribute "right-value" <| getValue themeMode gradient.right
    ,HtmlA.attribute "mode" <| String.toLower <| showChromaMode gradient.mode
    --,HtmlA.style "overflow-wrap" "normal"
    ] []
  ChromaName color -> Html.node "chroma-name"
    [HtmlA.attribute "username" username
    ,HtmlA.attribute "hue" <| String.fromInt color.hue
    ,HtmlA.attribute "chroma" <| getChroma themeMode color
    ,HtmlA.attribute "value" <| getValue themeMode color
    --,HtmlA.style "overflow-wrap" "normal"
    ] []

chromaBlock themeMode chromaColor attrs = Element.html <| Html.node "chroma-block"
    ([HtmlA.attribute "hue" <| String.fromInt chromaColor.hue
     ,HtmlA.attribute "chroma" <| getChroma themeMode chromaColor
     ,HtmlA.attribute "value" <| getValue themeMode chromaColor
     ] ++ attrs)
    []

getChroma themeMode color = String.fromInt <| if themeMode
  then color.chromaLight else color.chromaDark
getValue themeMode color = String.fromInt <| if themeMode
  then color.valueLight else color.valueDark


--------------------------------------------------------------------------------









--bubbleMain model =
--  let colorPalette = currentColorPalette model
--  in bubble model {ft = colorPalette.txSoft
--                  ,bg = colorPalette.bgMain
--                  ,highlight = colorPalette.mainHighlight}
--
--bubble model colors = tx >> el
--  ([paddingXY 6 8
--   ,Ft.color colors.ft, Ft.bold
--   ,Bg.color colors.bg
--   ,Bdr.rounded 12
--   ,mouseOver [Ft.color colors.bg
--              ,Bg.color colors.highlight]
--   ]
--  ++ buttonStyle
--  ++ colorTransitionStyle)




--bubbleActiveMain model active str = active |>
--  let colorPalette = currentColorPalette model
--  in bubbleActive model str
--       {ft = colorPalette.txSoft
--       ,bg = colorPalette.bgMain
--       ,activeBg = colorPalette.highlightBlue
--       ,highlight = colorPalette.mainHighlight
--       ,activeHighlight = colorPalette.highlightBlueBright}
--
--bubbleActive model str colors active = tx str |> el
--  ([paddingXY 6 8
--   ,Ft.bold
--   ,Bdr.rounded 12
--   ,mouseOver <| [Ft.color colors.bg
--                 ,Bg.color <| if active
--                     then colors.activeHighlight
--                     else colors.highlight]
--   ]
--   ++ buttonStyle
--   ++ colorTransitionStyle
--   ++ if active
--         then [Ft.color colors.bg
--              ,Bg.color colors.activeBg]
--         else [Ft.color colors.ft
--              ,Bg.color colors.bg])




bubbleUnactiveMain model unactive str = unactive |>
  let colorPalette = currentColorPalette model
  in bubbleUnactive model str
       {ft = colorPalette.txSoft
       ,bg = colorPalette.bgMain
       ,unactiveBg = colorPalette.bgMainDark
       ,highlight = colorPalette.mainHighlight}

bubbleUnactive model str colors unactive = tx str |> el
  ([paddingXY 6 8
   ,Ft.bold
   ,Bdr.rounded 12]
  ++ colorTransitionStyle
  ++ if unactive
        then [Ft.color colors.bg
             ,Bg.color colors.unactiveBg]
        else [Ft.color colors.ft
             ,Bg.color colors.bg
             ,mouseOver [Ft.color colors.bg
                        ,Bg.color colors.highlight]]
             ++ buttonStyle
             )


subPageButton model homePageInfo subPageInfo = el
  [Evt.onClick <| HomePageMsg <| if isSubPageOn homePageInfo.subPage subPageInfo
    then UnappendSubPage
    else AppendSubPage subPageInfo] <<
  bubbleActiveMain model (isSubPageOn homePageInfo.subPage subPageInfo)


fieldStyle model =
  let colorPalette = currentColorPalette model
  in [width fill
     ,Ft.color colorPalette.txSoft
     ,Bdr.width 2, Bdr.rounded 8, Bdr.color colorPalette.bgMainDark
     ,Bg.color colorPalette.bgMainDark
     ,focused [Bdr.color colorPalette.mainHighlight]
     ]

fieldCheckMain model inputField int onChange msgCheck =
  fieldCheck model inputField int <|
    \str -> BatchMsgs
      [onChange str
      ,WaitHalfASec <| msgCheck str
       {- after half a second, as long as the field input
          hasnt changed, we can ask the server -}
      ]

fieldCheckChatBox model inputField int onChange msgCheck =
  fieldCheck model inputField int <|
    \str -> BatchChatBoxMsgs
      [onChange str
      ,WaitHalfASecChatBox <| msgCheck str
       {- after half a second, as long as the field input
          hasnt changed, we can ask the server -}
      ]

fieldCheck model inputField int onChange textInput placeholderText inputText =
  let colorPalette = currentColorPalette model
  in inputField ([width fill
                 ,Ft.color colorPalette.txSoft
                 ,Bdr.width 2, Bdr.rounded 8
                 ,Bdr.color <| case int of
                    0 -> colorPalette.bgMainDark
                    1 -> colorPalette.highlightGreen
                    _ -> colorPalette.highlightRed
                 ,Bg.color colorPalette.bgMainDark
                 ,focused [Bdr.color <| case int of
                    0 -> colorPalette.mainHighlight
                    1 -> colorPalette.highlightGreen
                    _ -> colorPalette.highlightRed]
                 ]
                 ++ colorTransitionStyle)
                {onChange = onChange
                ,text = textInput
                ,placeholder = placeholder (Ft.color colorPalette.txSoft2) placeholderText
                ,label = In.labelHidden inputText
                }

fieldErrorText model =
  let colorPalette = currentColorPalette model
  in pt
  [paddingXY 8 0
  ,Ft.center, Ft.size 16, Ft.color colorPalette.highlightRed]




slidingHeader : Model -> Int -> Bool -> Bool ->
                       (Int -> a) -> (Bool -> Bool -> a -> msg) ->
                       List String -> Element msg
slidingHeader model currentHeaderSelection slideDirection initiateSlide
                    updateHeader slideMsg headers =
  let colorPalette = currentColorPalette model
      safeIndex int = Maybe.withDefault "" << List.Extra.getAt (int - 1)
      mod = List.length headers
      leftInt = modBy mod (modBy mod currentHeaderSelection + mod - 2) + 1
      rightInt = modBy mod currentHeaderSelection + 1
      slide = slideStyle slideDirection initiateSlide
      setHeader direction = slideMsg True direction << updateHeader
  in ro [width fill, Ft.size 24, Ft.bold] <|
        [el ([width <| fillPortion 1
            ,Ft.color colorPalette.txSoft3
            ,mouseOver [Ft.color colorPalette.txSoft2]
            ,pointer
            ,Evt.onClick <| setHeader True <| leftInt]
            ++ slide) <|
         el [centerX] <| tx <| safeIndex leftInt headers
        ,el ([width <| fillPortion 1
             ,Ft.color colorPalette.txSoft]
            ++ slide) <|
         el [centerX] <| tx <| safeIndex currentHeaderSelection headers
        ,el ([width <| fillPortion 1
             ,Ft.color colorPalette.txSoft3
             ,mouseOver [Ft.color colorPalette.txSoft2]
             ,pointer
             ,Evt.onClick <| setHeader False <| rightInt]
            ++ slide) <|
         el [centerX] <| tx <| safeIndex rightInt headers]







--------------------------------------------------------------------------------
-- poorly named



--mainBoxOverlayWrapper : Model -> Element Msg -> Element Msg
--mainBoxOverlayWrapper model panel =
--  let colorPalette = currentColorPalette model
--      close w h = el [width w, height h
--                     ,Evt.onClick <| HomePageMsg <| SetMainBoxOverlay NoMainBoxOverlay]
--                     Element.none
--      closeFill = close fill fill
--  in el [width fill, height fill
--        ,Bg.color <| rgba255 0 0 0 0.3] <|
--        co [width fill, height fill]
--           [close fill <| px 8
--           ,ro [width fill, height fill]
--               [closeFill
--               ,el [width <| px 484, height <| px 600, centerX, centerY
--                   ,Bdr.rounded 16
--                   ,htmlStyle "transition" "box-shadow 0.5s ease-in-out"
--                   ,htmlStyle "-webkit-transition" "box-shadow 0.5s ease-in-out"
--                   ,Bdr.shadow {offset = (0,0)
--                               ,size = 0
--                               ,blur = 5
--                               ,color = colorPalette.bgMain}
--                   ,mouseOver [Bdr.shadow {offset = (0,0)
--                               ,size = 0
--                               ,blur = 15
--                               ,color = colorPalette.bgMain}]
--                   ,Bg.color colorPalette.bgMain
--                   ,scrollbars] <|
--                   el [width fill, htmlStyle "height" "100%", htmlClass "hide-scroll", scrollbars]
--                      panel
--               ,closeFill]
--           ,closeFill]
--
---- TODO depricate
--mainBoxOverlayWrapper1 : Model -> Element Msg -> Element Msg
--mainBoxOverlayWrapper1 model panel =
--  let colorPalette = currentColorPalette model
--  in  co [width fill, height fill
--         ,Bg.color colorPalette.bgMain
--         ,scrollbars]
--         [el [width fill, height fill, scrollbars] <|
--             el [width fill, htmlStyle "height" "100%", htmlClass "hide-scroll", scrollbars]
--                panel
--         ,lazyEl ([width fill, alignBottom, paddingXY 0 6
--                  ,Ft.size 20, Ft.bold, Ft.color colorPalette.txSoft
--                  ,Bg.color colorPalette.bgMainDark
--                  ,Bdr.rounded 16
--                  ,mouseOver [Bg.color colorPalette.bgMainVeryDark]
--                  ,pointer
--                  ,Evt.onClick <| HomePageMsg <| SetMainBoxOverlay NoMainBoxOverlay]
--                 ++ colorTransitionStyle) <|
--                 el [centerX] <| tx "Close"]







--------------------------------------------------------------------------------
-- depricated

--fullscreenOverlayWrapper : Model -> Element Msg -> Element Msg
--fullscreenOverlayWrapper model panel =
--  let colorPalette = currentColorPalette model
--      close w h = el [width w, height h
--                     ,Evt.onClick <| HomePageMsg <| SetSubPageDefault True]
--                     Element.none
--      closeFill = close fill fill
--  in el [width fill, height fill
--        ,Bg.color <| rgba255 0 0 0 0.3] <|
--        co [width fill, height fill]
--           [-- TODO Clean this close element
--            el [width fill, height fill, paddingEach {bottom = 50, top = 0, left = 0, right = 0}
--               ,Evt.onClick <| HomePageMsg <| SetSubPageDefault True]
--               Element.none
--           ,ro [width fill, height fill]
--               [closeFill
--               ,el [width <| px 700, height <| px 900, centerX, centerY
--                   ,Bdr.rounded 16
--                   ,htmlStyle "transition" "box-shadow 0.5s ease-in-out"
--                   ,htmlStyle "-webkit-transition" "box-shadow 0.5s ease-in-out"
--                   ,Bdr.shadow {offset = (0,0)
--                               ,size = 0
--                               ,blur = 5
--                               ,color = colorPalette.bgMain}
--                   ,mouseOver [Bdr.shadow {offset = (0,0)
--                               ,size = 0
--                               ,blur = 15
--                               ,color = colorPalette.bgMain}]
--                   ,Bg.color colorPalette.bgMain
--                   ,scrollbars] <|
--                   el [width fill, htmlStyle "height" "100%", htmlClass "hide-scroll", scrollbars]
--                      panel
--               ,closeFill]
--           ,closeFill]



mkEmote : Int -> Emote -> Element msg
mkEmote textHeight emote =
  let emoteHeight = toFloat textHeight * 1.5
  in image [height <| px <| round emoteHeight
           ,width <| px <| round <|
             emote.image.height * emoteHeight / emote.image.width
           ,htmlStyle "vertical-align" "middle"
           --,htmlStyle "margin-top" <| "-" ++ String.fromFloat emoteHeight ++ "px"
           --,htmlStyle "top" <| String.fromFloat (toFloat textHeight / 1.6) ++ "px"
           ]
           {src = emote.image.url, description = emote.name}


selectionRow f options = ro [] <|
  flip List.indexedMap options <| \int -> f <|
    if int == 0 then Positive
       else if int == List.length options - 1
               then Negative else Neutral


selectionRowStyle model msg activity =
  let colorPalette = currentColorPalette model
  in [Evt.onClick msg]
     ++ buttonStyle
     ++ colorTransitionStyle
     ++ if activity.onOff then activity.on else activity.off


selectionRowStyleAdvacned model msg activity =
  let colorPalette = currentColorPalette model
  in [Evt.onClick msg
     ,mouseOver [Ft.color colorPalette.bgMainDark
                ,Bg.color colorPalette.mainHighlight
                ,Bdr.color colorPalette.mainHighlight]]
     ++ buttonStyle
     ++ colorTransitionStyle
     ++ if activity.isActive == Just True
           then activity.notActive ++ if activity.onOff then activity.onNotActive else activity.offNotActive
           else activity.active ++ if activity.onOff then activity.onActive else activity.offActive



overlayHeaderLeft model str =
  let colorPalette = currentColorPalette model
  in ro [width fill, paddingXY 16 0, spacing 8]
        [el [Ft.size 32, Ft.bold, Ft.color colorPalette.txSoft] <| tx str
        ,el [width fill] <| el [width fill, height <| px 4, Bg.color colorPalette.bgMain2] Element.none]


titleHeaderMiddle model str =
  let colorPalette = currentColorPalette model
  in el [centerX, Ft.size 32, Ft.bold, Ft.color colorPalette.txSoft] <| tx str
     --ro [width fill, paddingXY 16 0, spacing 16]
     --   [el [width fill] <| el [width fill, height <| px 4, Bg.color colorPalette.bgMain2] Element.none
     --   ,el [Ft.size 32, Ft.bold, Ft.color colorPalette.txSoft] <| tx str
     --   ,el [width fill] <| el [width fill, height <| px 4, Bg.color colorPalette.bgMain2] Element.none]


placeholder textColor = Just << In.placeholder [moveLeft 2, moveUp 2, textColor] << tx







--------------------------------------------------------------------------------


styleEmail = el [Ft.bold, htmlStyle "user-select" "text"] << tx

buttonStyle : List (Attribute msg)
buttonStyle =
  [pointer
  ,noSelection]

colorTransitionStyle : List (Attribute msg)
colorTransitionStyle =
  [htmlStyle "transition" "color .2s , background-color .2s, border-color .2s"
  ,htmlStyle "-webkit-transition" "color .2s, background-color .2s, border-color .2s"]
  --[htmlStyle "transition" "color 2s , background-color 2s, border-color 2s"
  --,htmlStyle "-webkit-transition" "color 2s, background-color 2s, border-color 2s"]

slideStyle direction initiateOverlayHeaderSliding =
  let slideDirection = if direction then "-100%" else "100%"
  in if initiateOverlayHeaderSliding
    then [htmlStyle "transform" <| "translate("++slideDirection++",0%)"
         ,htmlStyle "-webkit-transform" <| "translate("++slideDirection++",0%)"]
    else [htmlStyle "transform" "translate(0%,0%)"
         ,htmlStyle "-webkit-transform" "translate(0%,0%)"
         ,htmlStyle "transition" "transform .5s"
         ,htmlStyle "-webkit-transition" "transform .5s"]


rounedRowStyle : Bool3 -> Int -> List (Attribute Msg)
rounedRowStyle bool3 int =
  [case bool3 of
        Positive -> Bdr.roundEach {topLeft = int, bottomLeft = int
                                  ,topRight = 0, bottomRight = 0}
        Neutral -> Bdr.rounded 0
        Negative -> Bdr.roundEach {topLeft = 0, bottomLeft = 0
                                  ,topRight = int, bottomRight = int}]

topRoundRowStyle bool3 int =
 [case bool3 of
         Positive -> Bdr.roundEach {topLeft = int, bottomLeft = 0
                               ,topRight = 0, bottomRight = 0}
         Neutral -> Bdr.rounded 0
         Negative -> Bdr.roundEach {topLeft = 0, bottomLeft = 0
                                   ,topRight = int, bottomRight = 0}]


emoteStyle url name = el
  [height (px 19)] <|
  image [height <| px 28, width <| px 28
        ,htmlStyle "vertical-align" "middle"
        ,htmlStyle "margin-top" "-28px"
        ,htmlStyle "top" "10px"]
        {src = url, description = name}


turnOffElmUIBlueFocus =
  focused [Bdr.shadow {offset = (0,0), size = 0
                      ,blur = 0, color = rgba255 0 0 0 0}]



bothResult r = case r of
  Err a -> a
  Ok a -> a









elmUIVBar : Model -> (ElmBarMsg msg1 -> msg2) -> String -> ElmBar a
         -> Element msg1 -> Element msg2
elmUIVBar model liftElmBarMsg name elmBar content =
  let colorPalette = currentColorPalette model
      elmBarName = name ++ "elmbar"
      onScroll = htmlAttribute <| HtmlE.on "wheel" <|
        JD.succeed <| ElmBarWait <| GetElmBarViewport elmBarName OnScroll
  in
  --   el ([width fill, height fill, htmlClass "hide-scroll", scrollbars
  --       ,onScroll
  --       ]
  --       ++ case maybeViewport of
  --            Nothing -> []
  --            Just viewport -> List.singleton <| inFront <|
  --              let sceneRatio = viewport.viewport.height / viewport.scene.height
  --              in el [height fill, alignRight
  --                    ,onScroll] <|
  --                 el [paddingTop <| round <| viewport.viewport.y * sceneRatio] <|
  --                 em [width <| px 10, height <| px <| round <|
  --                       viewport.viewport.height * sceneRatio
  --                    --   * (viewport.viewport.height / viewport.scene.height)
  --                    ,Bg.color colorPalette.txSoft
  --                    ,Bdr.rounded 5
  --                    ,onScroll]
  --      )
  --<| el [width fill, height fill
  --      ,scrollbars, htmlClass "hide-scroll", htmlID elmBarName]
  --<|
     Element.map liftElmBarMsg
  <| el ([width fill, height fill, scrollbars
         ,onScroll]
         ++ case elmBar.autoScroll of
           Nothing -> []
           Just autoScroll-> List.singleton <| inFront <|
                el ([width fill, centerX, alignBottom
                    ,paddingEach {bottom = 8, left = 40, right = 40, top = 0}
                    ,htmlStyle "transition" "opacity .2s"
                    ,htmlStyle "-webkit-transition" "opacity .2s"]
                   ++ if autoScroll
                         then [alpha 0
                              ,htmlStyle "pointer-events" "none"]
                         else [alpha 0.9]
                   )
             <| el ([width fill, centerX, paddingXY 0 8
                    ,Bdr.rounded 16
                    ,Bg.color colorPalette.bgMain3]
                    ++ if autoScroll
                          then [defaultCursor]
                          else [pointer, Evt.onClick <| GetElmBarViewport elmBarName <|
                                 AutoScrollDown True]
                   )
             <| ex [centerX, centerY
                   ,Ft.bold, Ft.size 16, Ft.color colorPalette.txSoft]
                   "Scroll Down"
        )
  <| el [width fill, height fill
        ,scrollbars, htmlID elmBarName]
  <| Element.map ElmBarMsg content
