{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Pages where

import Data.Text as Text

import Import

import Internal.RenderElmApp

-- Power 0

getHomePageR :: Handler Html
getHomePageR = do
  renderElmApp

getChatPageR :: Handler Html
getChatPageR = do
  renderElmApp

getChatStreamPageR :: Handler Html
getChatStreamPageR = do
  renderElmApp




--------------------------------------------------------------------------------
-- Power 1




getModPageR :: Handler Html
getModPageR = do
  renderElmApp

getStreamerPageR :: Handler Html
getStreamerPageR = do
  renderElmApp




--------------------------------------------------------------------------------
-- Power 2




getAdminPageR :: Handler Html
getAdminPageR = do
  renderElmApp
