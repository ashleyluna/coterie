module Main.Update exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Maybe.Extra as MaybeE
import Result.Extra as ResultE
import String.Extra as StringE
import Task
import Time

import Accessors exposing (..)

import Internal.Internal exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)
import Update.Chat exposing (..)
import Update.CommonInfo exposing (..)
import Update.LiveInfo exposing (..)
import Update.Socket exposing (..)
import Update.Page exposing (..)

{-
Update is broken up into multiple files so similar Msg's or ones what call each
other will easier to read together, than just with whitespace.

Each updater function covers certain Msg's, if it doesn't, we move onto the next
one function. If the Msg is undefined, or is NoMsg (NoMsg represents an undefined Msg),
then we dont de anything

socketReceiver covers messges received over the socket on the js side and
returns Cmd's to batch more Msg's

the others are defined based on sections of the model adn cover updates over
those sections
-}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = List.foldr
  (\f -> f model msg)
  (model, Cmd.none)
  [generalMsgs
  ,socketUpdates
  ,updateLiveInfo
  ,updateCommonInfo
  ,updatePage
  -- for unorganized Msg's
  --,updateEverythingElse
  ]


generalMsgs : Update
generalMsgs model msg next =
  let setUpLens = makeOneToOne .setUp <|
            \change record -> {record | setUp = change record.setUp}
      overSetUp f = over setUpLens f model
  in case msg of
       BatchMsgs msgs -> pair model <| Cmd.batch <| List.map cmdMsg msgs
       MsgCmd cmd -> (model, cmd)
       UseNow f -> pair model <| do <| flip Task.map Time.now f
       --SetUp -> (model, Cmd.none) -- TODO
       LogMessage str -> pair model <| logMessage str
       WaitHalfASec message -> pair model <| do <|
         waitHalfASec <| Task.succeed <| message
       SetPage page -> case page of
         HomePage pageInfo -> pair {model | page = HomePage pageInfo} <|
           cmdMsg <| HomePageMsg CheckStreamTitleLength
         pageInfo -> noCmd {model | page = pageInfo}
       SetUpProfile -> noCmd <| overSetUp <| \setup -> {setup | profile = True}
       --SetUpProfile -> noCmd <| overSetUp <| \setup -> {setup | profile = True}
       ApiREQUEST method body headers apiPath requestType decoder expectResponse -> pair model <|
         let maybeCSRFToken = Result.toMaybe <| getCsrfToken model.commonInfo.localInfo.cookie
         in Http.request
             {method = method
             ,headers = headers ++ MaybeE.unwrap []
               (List.singleton << Http.header "X-CSRFTOKEN") maybeCSRFToken -- "X-XSRF-TOKEN"
             ,timeout = Just 5000
             ,tracker = Nothing
             ,url = apiUrl model.commonInfo.localInfo.url ++ apiPath
             ,body = body
             ,expect = Http.expectJson expectResponse <| JD.map (\msg_ ->
               BatchMsgs <| [msg_] ++ if maybeCSRFToken /= Nothing then []
                 else [LogMessage <| "Couldn't Read CSRF Token: " ++ model.commonInfo.localInfo.cookie]) <|
               JD.oneOf
               [JD.map (LogMessage << (++) "API Error: ") (JD.field "error" JD.string)
               ,flip JD.andThen (JD.field "type" JD.string) <|
                 \str -> if str == requestType
                    then decoder
                    else JD.fail "Wrong Response Message Type"]
             }
       ApiGET headers apiPath responseType decoder expectResponse -> pair model <| cmdMsg <|
         ApiREQUEST "GET"
                    Http.emptyBody
                    headers apiPath responseType decoder expectResponse
       ApiPOST headers apiPath requestType body decoder expectResponse -> pair model <| cmdMsg <|
         ApiREQUEST "POST"
                    (Http.jsonBody <| JE.object <|
                      pair "type" (JE.string requestType) :: body)
                    headers apiPath requestType decoder expectResponse
       _ -> next
