{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Widgets.Navigation where

import API
import Reflex.Dom hiding (Home)

navbar = divClass "drawer" $ do
    home <- elClass "h4" "home-link" $ linkFor Home $ link "smash.gg"
    others <- elClass "ul" "fa-ul" $ leftmost <$> sequence
        [ el "li" $ do
            linkFor Tournaments $ do
                elClass "i" "fa-li fa fa-calendar" blank
                elClass "i" "fa fa-circle" blank
                text "Tournaments"
        , el "li" $ do
            linkFor Results $ do
                elClass "i" "fa-li fa fa-trophy" blank
                elClass "i" "fa fa-circle" blank
                text "Results"
        , el "li" $ do
            linkFor Rankings $ do
                elClass "i" "fa-li fa fa-rocket" blank
                elClass "i" "fa fa-circle" blank
                text "Rankings"
        , el "li" $ do
            elClass "i" "fa-li fa fa-user" blank
            linkFor UserHome $ text "Xanax"
        ]
    return $ leftmost [home, others]
    where
        linkFor p t = do
            (a, _) <- el' "a" t
            return $ p <$ domEvent Click a

