module Update.ElmBar exposing (..)

import Browser.Dom as Dom exposing (Viewport)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task
import Time exposing (Posix, Zone)

import Accessors exposing (..)
import String.Extra exposing (clean)

import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)






updateElmBar_ : UpdateElement (ElmBarMsg msg) (ElmBar ())
updateElmBar_ = updateElmBar <| \model innerMsg _ liftMsg emptyContentUpdate ->
  (emptyContentUpdate (), cmdMsg <| liftMsg innerMsg)

updateElmBar : (UpdateElement msg a) -> UpdateElement (ElmBarMsg msg) (ElmBar a)
updateElmBar innerUpdate model msg elmBar liftElmBarMsg setElmBar =
  let a = 1
  in case msg of
       ElmBarMsg innerMsg -> innerUpdate model innerMsg elmBar.content
         (liftElmBarMsg << ElmBarMsg)
         <| \newContent -> setElmBar {elmBar | content = newContent}
       BatchElmBarMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftElmBarMsg) msgs
       GetElmBarViewport str next -> pair model <|
         flip Task.attempt (Dom.getViewportOf str) <|
           \res -> case res of
              Ok viewport -> liftElmBarMsg <| next str viewport
              Err _ -> NoMsg
       SetElmBarViewPort viewport -> noCmd <| setElmBar {elmBar | viewport = Just viewport}
       OnScroll str viewport -> noCmd <| setElmBar <|
         let -- the distance between the bottom of the viewport and the bottom of the scene
             bottomMargin = viewport.scene.height
                          - (viewport.viewport.height + viewport.viewport.y)
         in {elmBar | viewport = Just viewport
                    , infiniteScroll = flip Maybe.map elmBar.infiniteScroll <|
                        \infiniteScroll -> {infiniteScroll | stack =
                           let addZone = if infiniteScroll.direction
                                 then viewport.viewport.y
                                 else bottomMargin
                           in infiniteScroll.stack +
                                if 100 >= addZone then 1 else 0
                        }
                    , autoScroll = flip Maybe.map elmBar.autoScroll <|
                        \_ -> 100 >= bottomMargin
            }
       ResetScrollStack -> noCmd <| setElmBar
         {elmBar | infiniteScroll = flip Maybe.map elmBar.infiniteScroll <|
                     \infiniteScroll -> {infiniteScroll | stack = 1}
         }
       AutoScrollDown force str viewport ->
         (setElmBar {elmBar | autoScroll = Maybe.map ((||) force) elmBar.autoScroll}
         ,let bottom = viewport.scene.height - viewport.viewport.height
              newViewport = {viewport | viewport =
                let innerViewport = viewport.viewport
                in {innerViewport | y = if force || elmBar.autoScroll /= Just True
                     then innerViewport.y else bottom}}
          in cmdIf (force || elmBar.autoScroll == Just True) <|
               flip Task.attempt (Dom.setViewportOf str 0 bottom) <|
                 \_ -> liftElmBarMsg <| SetElmBarViewPort newViewport
         )
       ElmBarWait elmBarMsg -> pair model <|
         do <| wait 300 <| Task.succeed <| liftElmBarMsg elmBarMsg
