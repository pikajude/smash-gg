{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           API
import qualified Data.Map           as M
import           Data.Monoid
import           Data.Text.Encoding
import           Head
import           Reflex.Dom         hiding (Home)

import qualified Pages.Home
import qualified Pages.Rankings
import qualified Pages.Results
import           Widgets.Navigation

main = mainWidgetWithHead' $ (,) pageHead $ \ _ -> do
    navigationEvent <- navbar

    divClass "main-wrapper clearfix" $
        widgetHold Pages.Home.page $ pageMap <$> navigationEvent

homePage = divClass "columns" $ text "Hello, world!"

pageMap Home     = Pages.Home.page
pageMap Results  = Pages.Results.page
pageMap Rankings = Pages.Rankings.page

pageHead _ = do
    el "title" $ text "Hello, world!"
    elAttr "style" ("type" =: "text/css") $ text $ decodeUtf8 css
