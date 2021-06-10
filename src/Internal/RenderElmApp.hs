{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Internal.RenderElmApp where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist

import Import
import qualified Model as DB

import Internal.Api
import Internal.User



renderElmApp :: Handler Html
renderElmApp = do
  App {..} <- getYesod
  let Secrets {..} = streamerSecrets
  flags <- mkFlags
  streamStatus <- toJSON <$> readTVarIO tvStreamStatus
  pc <- widgetToPageContent $ wBodyScript flags streamStatus streamerInfo
  withUrlRenderer $ [hamlet|
      $doctype 5
      <html>
        <head>
          <meta charset=utf-8>
          <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato|Roboto|Open Sans">
          <link rel=stylesheet href="/static/css/all.min.css">
          <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/simplebar@latest/dist/simplebar.css"/>
          <script src="https://cdn.jsdelivr.net/npm/simplebar@latest/dist/simplebar.min.js">
          <script src="https://cdn.jsdelivr.net/npm/underscore@1.12.0/underscore-min.js">
          <script src="https://cdnjs.cloudflare.com/ajax/libs/chroma-js/2.1.1/chroma.min.js">
          <meta name="google-signin-client_id" content=#{googleClientId}>
          <script src="//ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js">
          <script src="https://apis.google.com/js/client:platform.js?onload=loadGoogleApi" async defer>
          <script src="https://apis.google.com/js/api.js">
          <script>
            function loadGoogleApi()
              {gapi.load('auth2', function()
                {auth2 = gapi.auth2.init(
                  {client_id: "#{googleClientId}"
                  ,scope: "email"
                  });});};
          <script type="text/javascript" src="/static/SerfsTest.js">
          <style>
            .hide-scroll::-webkit-scrollbar
              {display: none}
            .hide-scroll
              {-ms-overflow-style: none
              ;scrollbar-width: none}
        <body>
          ^{pageBody pc}
    |]



--------------------------------------------------------------------------------



mkFlags :: Handler Value
mkFlags = do
  App {..} <- getYesod
  let StreamerInfo {..} = streamerInfo
  specialRoles <- do specialRoles <- readTVarIO tvSpecialRoles
                     return $ HashMap.fromListWith max
                            $ (\(SpecialRole name power) -> (name, power))
                          <$> HashMap.elems specialRoles


  specialRoleBadges <- HashMap.elems <$> readTVarIO tvSpecialRoleBadges
  subBadges <- fmap IntMap.elems <$> readTVarIO tvSubBadges
  seasonBadges <- readTVarIO tvSeasonBadges
  commonBadges <- readTVarIO tvCommonBadges
  globalEmoteList <- HashMap.elems <$> readTVarIO tvGlobalEmoteList
  subOnlyEmoteList <- HashMap.elems <$> readTVarIO tvSubOnlyEmoteList
  return $ object
    ["static_info" .= object
      ["special_roles" .= specialRoles
      ,"special_role_badges" .= specialRoleBadges
      ,"sub_badges" .= subBadges
      ,"season_badges" .= seasonBadges
      ,"common_badges" .= commonBadges
      ,"global_emote_list" .= globalEmoteList
      ,"sub_only_emote_list" .= subOnlyEmoteList
      ,"points_rewards" .= object []
      ]
    ]

--mkLiveInfo :: StreamStatus -> Value
--mkLiveInfo streamStatus {-usersInChat-} = object
--  ["streamStatus" .= toJSON streamStatus
--  ,"usersInChat" .= object [] -- toJSON usersInChat
--  ]




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




wBodyScript flags streamStatus streamerInfo = toWidgetBody $ [julius|
var settingsData = localStorage.getItem("settings")
;
var settings = settingsData ? JSON.parse(settingsData) : null
;
var flags = #{flags}
;
flags.cookie = document.cookie
;
flags.settings = settings
;
var app = Elm.Main.init({
  // node: document.getElementById("myapp")
  flags: flags
  })
;




////////////////////////////////////////////////////////////////////////////////




// ELM PORTS
// log message
app.ports.logMessage.subscribe(function(message)
  {console.debug(message)
  ;})
;
// request cookie
app.ports.requestCookie.subscribe(function()
  {app.ports.receiveCookie.send(document.cookie)
  ;})
;
// socket
var chatRoomSocket = new WebSocket("ws://" + #{rootUrl streamerInfo} + "/ws")
;
app.ports.sendOverSocket.subscribe(function(message)
  {console.debug(message)
  ;chatRoomSocket.send((message))
  ;})
;
chatRoomSocket.addEventListener("message", function(event)
  {console.debug(event)
  ;app.ports.receiveOverSocket.send(event.data)
  ;})
;
// local storage
app.ports.setSettingsStorage.subscribe(function(state)
  {localStorage.setItem("settings", state)
  ;})
;
// google sign in
app.ports.startGoogleSignIn.subscribe(function()
  {auth2.grantOfflineAccess().then(app.ports.postGoogleSignIn.send)
  ;})
;




////////////////////////////////////////////////////////////////////////////////




// Chroma
function mkColor(hue,chroma,value)
  {return {l: parseInt(value), c: parseInt(chroma), h: parseInt(hue)};}
;
// chroma block
customElements.define("chroma-block", class extends HTMLElement {
  constructor() {super();}
  connectedCallback() {this.mkChromaBlock();}
  attributeChangedCallback() {this.mkChromaBlock();}
  static get observedAttributes()
    {return ["hue","chroma","value"];}
  mkChromaBlock()
    {const hue = this.getAttribute("hue")
    ;const chr = this.getAttribute("chroma")
    ;const value = this.getAttribute("value")
    ;this.style.backgroundColor = chroma.lch(mkColor(hue,chr,value))
    ;}
  })
;
// chroma name
customElements.define("chroma-name", class extends HTMLElement {
  constructor() {super();}
  connectedCallback() {this.mkChromaName();}
  attributeChangedCallback() {this.mkChromaName();}
  static get observedAttributes()
    {return ["username","hue","chroma","value"];}
  mkChromaName()
    {const username = this.getAttribute("username")
    ;const hue = this.getAttribute("hue")
    ;const chr = this.getAttribute("chroma")
    ;const value = this.getAttribute("value")
    ;this.innerHTML = `<span style="color: ${chroma.lch(mkColor(hue,chr,value))}">${username}</span>`
    ;}
  })
;
// chroma gradient name
customElements.define("chroma-name-gradient", class extends HTMLElement {
  constructor() {super();}
  connectedCallback() {this.mkChromaNameGradient();}
  attributeChangedCallback() {this.mkChromaNameGradient();}
  static get observedAttributes()
    {return ["username","left-hue","left-chroma","left-value"
            ,"right-hue","right-chroma","right-value","mode"];}
  mkChromaNameGradient()
    {const username = this.getAttribute("username")
    ;const hueLeft = this.getAttribute("left-hue")
    ;const chromaLeft = this.getAttribute("left-chroma")
    ;const valueLeft = this.getAttribute("left-value")
    ;const hueRight = this.getAttribute("right-hue")
    ;const chromaRight = this.getAttribute("right-chroma")
    ;const valueRight = this.getAttribute("right-value")
    ;const mode = this.getAttribute("mode")
    ;const colorList = [mkColor(hueLeft,chromaLeft,valueLeft)
                       ,mkColor(hueRight,chromaRight,valueRight)]
    ;const scale = chroma.scale(colorList).mode(mode).colors(username.length)
    ;function colorChar(nameChars, cList)
      {return (nameChars.length < 1 || cList.length < 1 ? `` :
        `<span style="color: ${_.head(cList)}">${_.head(nameChars)}</span>${colorChar(_.tail(nameChars),_.tail(cList))}`)
      ;}
    ;this.innerHTML = colorChar(username.split(""), scale)
    ;}
  })
;
|]
