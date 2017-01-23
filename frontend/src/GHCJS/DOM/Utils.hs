{-# LANGUAGE FlexibleContexts #-}

module GHCJS.DOM.Utils where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified GHCJS.DOM              as DOM
import qualified GHCJS.DOM.Document     as DOM
import qualified GHCJS.DOM.Element      as DOM
import qualified GHCJS.DOM.EventM       as DOM
import qualified GHCJS.DOM.Window       as DOM
import           Reflex.Dom

windowSize = do
    wv <- askWebView
    Just doc <- liftIO $ DOM.webViewGetDomDocument $ unWebViewSingleton wv
    Just window <- liftIO $ DOM.getDefaultView doc
    ev <- wrapDomEvent window (`DOM.on` DOM.resize) $ dimensions window
    pb <- delay 0 =<< getPostBuild
    ev2 <- performEvent $ ffor pb $ \ _ -> dimensions window
    initial <- dimensions window
    holdDyn initial $ leftmost [ev, ev2]
    where
        dimensions w = liftA2 (,) (DOM.getInnerWidth w) (DOM.getInnerHeight w)

elementSize e = do
    resize <- windowSize
    sizeEv <- performEvent $ ffor (updated resize) $ \ _ -> owidth
    holdDyn 0 sizeEv
    where
        owidth = liftIO $ DOM.getOffsetWidth $ _element_raw e
