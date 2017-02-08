{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Widgets.Navigation where

import API
import Data.Monoid
import Data.Text   (Text)
import Reflex.Dom  hiding (Home)

data Item = Item
          { dest      :: Page
          , linkText  :: Text
          , liClass   :: Text
          , iconClass :: Text
          }

renderItem Item{..} = elClass "li" liClass $ linkFor dest $ do
    elClass "i" ("fa " <> iconClass) blank
    elClass "i" "fa fa-circle" blank
    el "span" $ text linkText
    elClass "div" "tooltip" $ do
        elClass "span" "tooltip-arrow" blank
        text linkText

linkFor p t = do
    (a, _) <- el' "a" t
    return $ p <$ domEvent Click a

navbar = divClass "drawer" $ do
    home <- elClass "h4" "home-link" $ linkFor Home $ do
        el "span" $ text "smash."
        text "gg"
    others <- elClass "ul" "navigation" $ leftmost <$> mapM renderItem
        [ Item Tournaments "Tournaments" "li-tournaments" "fa-calendar"
        , Item Results "Results" "li-results" "fa-trophy"
        , Item Rankings "Rankings" "li-rankings" "fa-rocket"
        , Item UserHome "Xanax" "li-user" "fa-user"
        ]
    return $ leftmost [home, others]
