{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           API                    hiding (Event)
import qualified API                    (Event)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.Map               as M
import           Data.Monoid
import qualified Data.Serialize         as S
import           Data.Text.Encoding
import           GHCJS.DOM.Types        (IsElement)
import           Reflex.Dom             hiding (Home)
import           Web.Routes.PathInfo

import           Head
import qualified Pages.Home
import qualified Pages.Rankings
import qualified Pages.Results
import           Router
import           Widgets.Navigation

main = mainWidgetWithHead' $ (,) pageHead $ \ _ -> do
    topEvent <- navbar
    postBuild <- getPostBuild

    rec routeText <- partialPathRoute "" (toPathInfo <$> pageNav)
        let route :: Dynamic _ (Either String API.Request)
            route = parseSegments fromPathSegments <$> routeText

            firstPage = fmapMaybe getRight $ tag (current route) postBuild
            messageEv = leftmost [ fmapMaybe getRight (updated route)
                                 , firstPage
                                 ]

        (badMsg, goodMsg) <- connection messageEv

        pageNav <- divClass "main-wrapper clearfix" $ do
            rec pageNavEvent <- widgetHold initialWidget $ pageMap <$> goodMsg

                let navigationEvent = leftmost [topEvent, switch $ current pageNavEvent]

            return navigationEvent

    return ()
    where
        getRight = either (const Nothing) Just
        initialWidget = text "Loading..." >> return never

connection sendMessage = do
    serverConn <- webSocket "ws://localhost:8000"
        (def { _webSocketConfig_send = ((:[]) . S.encode) <$> sendMessage })
    let dataEv :: Event _ (Either String API.Response)
        dataEv = S.decode <$> _webSocket_recv serverConn
    return (fmapMaybe getLeft dataEv, fmapMaybe getRight dataEv)
    where
        getLeft = either Just (const Nothing)
        getRight = either (const Nothing) Just

pageMap :: (Reflex t, MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, MonadIO m
           , IsElement (RawElement (DomBuilderSpace m)), HasWebView m, TriggerEvent t m
           , MonadIO (Performable m), PerformEvent t m)
        => API.Response -> m (Event t API.Request)
pageMap p@HomeR{}   = Pages.Home.page p
pageMap p@ResultsR  = Pages.Results.page p
pageMap p@RankingsR = Pages.Rankings.page p

pageHead _ = do
    el "title" $ text "smash.gg"
    elAttr "style" ("type" =: "text/css") $ text $ decodeUtf8 css
