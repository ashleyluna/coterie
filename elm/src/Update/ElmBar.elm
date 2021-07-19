module Update.ElmBar exposing (..)

import Browser.Dom as Dom exposing (Viewport)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task
import Time exposing (Posix, Zone)

import String.Extra exposing (clean)

import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)






--updateElmBar_ : UpdateElement ElmBarMsg ElmBar
--updateElmBar_ = updateElmBar <| \model innerMsg _ liftMsg emptyContentUpdate ->
--  (emptyContentUpdate (), cmdMsg <| liftMsg innerMsg)

--updateElmBar : ElmBar -> (ElmBar -> Model) -> (ElmBarMsg msg -> Msg) -> GeneralMsgUpdate (ElmBarMsg msg)
updateElmBar elmBar setElmBar liftMsg model msg liftElmBarMsg =
  updateGeneralMsg model msg liftElmBarMsg <|
    updateElmBar_ elmBar setElmBar liftMsg
--updateElmBar : (UpdateElement msg a) -> UpdateElement ElmBarMsg ElmBar
--updateElmBar_ : (msg -> Msg) -> UpdateElement (ElmBarMsg msg) ElmBar
updateElmBar_ elmBar setElmBar liftMsg model msg liftElmBarMsg =
  let a = 1
  in case msg of
       --ElmBarMsg innerMsg -> innerUpdate model innerMsg elmBar.content
       --  (liftElmBarMsg << ElmBarMsg)
       --  <| \newContent -> setElmBar {elmBar | content = newContent}
       --BatchElmBarMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftElmBarMsg) msgs
       GetElmBarViewport elmBarName next -> pair model <|
         flip Task.attempt (Dom.getViewportOf elmBarName) <|
           \res -> case res of
              Ok viewport -> liftMsg <| next viewport
              Err _ -> NoMsg
       SetElmBarViewPort viewport -> noCmd <| setElmBar
         {elmBar | viewport = Just viewport}
       UpdateAutoScroll viewport -> noCmd <| setElmBar <|
         {elmBar | autoScroll = flip Maybe.map elmBar.autoScroll <|
           -- the distance between the bottom of the viewport and the bottom of the scene
           \_ -> 100 >= viewportMarginDown viewport}
       --OnScroll elmBarName viewport -> noCmd <| setElmBar <|
       --  let -- the distance between the bottom of the viewport and the bottom of the scene
       --      bottomMargin = viewportMarginDown viewport
       --  in {elmBar | viewport = Just viewport
       --             , infiniteScroll = flip Maybe.map elmBar.infiniteScroll <|
       --                 \infiniteScroll -> {infiniteScroll | stack =
       --                    let addZone = if infiniteScroll.direction
       --                          then viewport.viewport.y else bottomMargin
       --                    in infiniteScroll.stack +
       --                         if 100 >= addZone then 1 else 0
       --                 }
       --             , autoScroll = flip Maybe.map elmBar.autoScroll <|
       --                 \_ -> 100 >= bottomMargin
       --     }
       --ResetScrollStack -> noCmd <| setElmBar
       --  {elmBar | infiniteScroll = flip Maybe.map elmBar.infiniteScroll <|
       --              \infiniteScroll -> {infiniteScroll | stack = 1}
       --  }
       AutoScrollDown force elmBarName viewport ->
         (setElmBar {elmBar | autoScroll = Maybe.map ((||) force) elmBar.autoScroll}
         ,let bottom = viewport.scene.height - viewport.viewport.height
              newViewport = {viewport | viewport =
                let innerViewport = viewport.viewport
                in {innerViewport | y = if force || elmBar.autoScroll /= Just True
                     then innerViewport.y else bottom}}
          in cmdIf (force || elmBar.autoScroll == Just True) <|
               flip Task.attempt (Dom.setViewportOf elmBarName 0 bottom) <|
                 \_ -> liftElmBarMsg <| Msg <| SetElmBarViewPort newViewport
         )
       --ElmBarWait elmBarMsg -> pair model <|
       --  do <| wait 300 <| Task.succeed <| liftElmBarMsg elmBarMsg
