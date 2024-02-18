{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Control.Monad.IO.Class

import EventListener


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      endedEvent {-:: Dynamic t (Event t a)-} <- prerender (pure never) $ do
        (e, _) <- elAttr' "audio" ("id" =: "myAudioEl") blank
        ended <- addHTMLEventListener'' e "end" $ \ev trigger -> do
          --clog "hey"
          liftIO $ trigger "hey" 
        pure ended
  
      -- switchDyn :: Dynamic t (Event t a) -> Event t a
      let end' = switchDyn endedEvent
      
      
      --(e, _) <- elAttr' "audio" ("id" =: "myAudioEl") blank
      return ()
  }




