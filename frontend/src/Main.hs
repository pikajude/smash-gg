{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           API
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.Map               as M
import           Data.Monoid
import           Data.Text.Encoding
import           GHCJS.DOM.Types        (IsElement)
import           Head
import           Reflex.Dom             hiding (Home)

import qualified Pages.Home
import qualified Pages.Rankings
import qualified Pages.Results
import           Widgets.Navigation

main = mainWidgetWithHead' $ (,) pageHead $ \ _ -> do
    rec topEvent <- navbar

        pageNavEvent <- divClass "main-wrapper clearfix" $
             widgetHold Pages.Home.page $ pageMap <$> navigationEvent

        let navigationEvent = leftmost [topEvent, switch $ current pageNavEvent]
    return ()

pageMap :: (Reflex t, MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, MonadIO m
           , IsElement (RawElement (DomBuilderSpace m)), HasWebView m, TriggerEvent t m
           , MonadIO (Performable m), PerformEvent t m)
        => Page -> m (Event t Page)
pageMap Home     = Pages.Home.page
pageMap Results  = Pages.Results.page
pageMap Rankings = Pages.Rankings.page

pageHead _ = do
    el "title" $ text "Hello, world!"
    elAttr "style" ("type" =: "text/css") $ text $ decodeUtf8 css
