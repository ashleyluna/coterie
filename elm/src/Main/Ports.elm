port module Main.Ports exposing (..)

import Json.Decode as JD
import Json.Encode as JE

type alias Send msg a = a -> Cmd msg
type alias Receive msg a = (a -> msg) -> Sub msg

port requestCookie : Send msg ()
port receiveCookie : Receive msg String

port sendOverSocket : Send msg String
port receiveOverSocket : Receive msg String

port setSettingsStorage : Send msg String

port startGoogleSignIn : Send msg ()
port postGoogleSignIn : Receive msg JD.Value

-- for debugging
port logMessage : Send msg String
