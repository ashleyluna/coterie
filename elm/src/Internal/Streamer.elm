module Internal.Streamer exposing (..)

import Array exposing (Array)

import Element exposing (..)

import Main.Model exposing (..)
import Main.Msg exposing (..)

type alias Streamer =
  {siteTitle : String
  ,name : String
  ,websiteAdminName : String
  ,plural : Bool
  ,streamChatSettings : SettingsRecord
  ,emails : StaticEmails
  ,logo : Image
  ,mainPageLinks : List MainPageLink
  ,navBarLinks : List NavBarLink
  ,socialLinks : List NavBarLink
  ,subscriptionTiersLogos : Array Emote
  ,displayEmote : Emote -- display this emote in the subscription subPage
  ,defaultRole : String
  ,seasonDates : List String
  ,currentSeason : Int
  ,colors : Colors
  ,secrets :
    {twitchClientId : String
    }
  }


type alias StaticEmails =
  {support : String
  ,contact : String
  ,feedback : String
  ,unban : String
  ,reporting : String
  ,websiteAdmin : String
  }


type alias MainPageLink =
  {url : String
  ,logo : Emote}

type alias NavBarLink =
  {title : String
  ,url : String}


type alias Colors =
  {lightMode : ColorPalette
  ,darkMode : ColorPalette}

type alias ColorPalette =
  {bgMain : Color
  ,bgMainDark : Color
  ,bgMainVeryDark : Color
  ,bgMain2 : Color
  ,bgMain3 : Color
  ,txMain : Color
  ,txMain2 : Color
  ,txSoft : Color
  ,txSoftDark : Color
  ,txSoft2 : Color
  ,txSoft3 : Color
  ,mainHighlight : Color
  ,highlightBlueBright : Color
  ,highlightBlue : Color
  ,highlightGreen : Color
  ,highlightRed : Color
  ,alpha : Float
  }
