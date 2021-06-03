module Update.CommonInfo exposing (..)

import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task
import Url

import Element exposing (Color, rgb255)

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)
import Streamer exposing (..)


updateCommonInfo : Update
updateCommonInfo model msg next =
  let setCommonInfo info = {model | commonInfo = info}
      overCommonInfo f = {model | commonInfo = f model.commonInfo}
      overLocalInfo f = overCommonInfo <| \commonInfo ->
        {commonInfo | localInfo = f commonInfo.localInfo}
      overStaticInfo f = overCommonInfo <| \commonInfo ->
        {commonInfo | staticInfo = f commonInfo.staticInfo}
  in case msg of
       -- Local Info
       RequestUrl (Browser.Internal url) -> Debug.log "Internal" <| pair model <| Nav.load <| Url.toString url
       RequestUrl (Browser.External url) -> Debug.log "External" <| pair model <| Nav.load url
       ChangeUrl urlChange -> noCmd model
       SetZone zone -> noCmd <| overLocalInfo <| \localInfo ->
         {localInfo | zone = Just zone}
       GetCookie -> pair model <| requestCookie ()
       ReceiveCookie str -> pair (overLocalInfo <| \localInfo ->
         {localInfo | cookie = str})
         <| cmdMsg <| LogMessage <| "new cookie " ++ str

       -- Main User
       GETUserMe -> pair model <| cmdMsg <| ApiGET [] "profile" "get_profile"
         (JD.map (SetProfile << Just) jdProfile) <|
         \response -> BatchMsgs
           [SetUpProfile
           ,case response of
               Err err -> Debug.log "Error GETUserMe" <| httpErrorMsg err
               Ok resMsg -> Debug.log "Success GETUserMe" <| resMsg]
       GETLogOut -> pair model <| cmdMsg <| ApiGET [] "profile/logout" "log_out"
         (JD.succeed <| MsgCmd Nav.reload) <|
         \response -> case response of
             Err err -> httpErrorMsg err
             Ok resMsg -> resMsg
       POSTGoogleSignIn value ->
         let decoder = JD.map
               (\code -> {code = code})
               (JD.field "code" JD.string)
         in pair model <| case JD.decodeValue decoder value of
              Ok googleSignIn -> cmdMsg <| ApiPOST [] "register" "google_sign_in"
                [pair "code" <| JE.string googleSignIn.code]
                (JD.succeed NoMsg)
                <| \response -> NoMsg
              Err err -> logMessage <| JD.errorToString err
       StartTwitchSignIn -> pair model <| Http.request
         {method = "GET"
         ,headers =
           [Http.header "client_id" "6mgg169rdswvxgdoyvnojqdllx9hu3"
           ,Http.header "redirect_uri" "http://localhost:3000"
           ,Http.header "response_type" "code"
           ,Http.header "scope" "user:read:email"]
         ,url = "https://id.twitch.tv/oauth2/authorize"
         ,body = Http.emptyBody
         ,timeout = Nothing
         ,tracker = Nothing
         ,expect = Http.expectWhatever <| \_ -> NoMsg
         }
       SetProfile profile -> (overCommonInfo <| \commonInfo ->
          {commonInfo | profile = profile}
         ,cmdMsg SetUpProfile)
       OverProfile f -> (overCommonInfo <| \commonInfo ->
          {commonInfo | profile = Maybe.map f commonInfo.profile}
         ,cmdMsg SetUpProfile)
       SetSettings settings -> (overCommonInfo <| \commonInfo ->
          {commonInfo | settings = settings}
         ,setSettingsStorage <| encodeMainUserSettings settings)
       _ -> next







commonInfoSetUp =
  [cmdMsg GETUserMe
  ]




commonInfoSubBatch commonInfo =
  [
  ]
