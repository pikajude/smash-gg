{-# OPTIONS_GHC -Wall #-}

module Widgets.Navigation where

import API
import Reflex.Dom hiding (Home)

navbar = divClass "drawer" $ do
    home <- elClass "h4" "home-link" $ linkFor Home $ link "smash.gg"
    others <- elClass "ul" "fa-ul" $ leftmost <$> sequence
        [ el "li" $ do
            elClass "i" "fa-li fa fa-calendar" blank
            linkFor Tournaments $ do
                elClass "i" "fa fa-circle" blank
                link "Tournaments"
        , el "li" $ do
            elClass "i" "fa-li fa fa-trophy" blank
            linkFor Results $ do
                elClass "i" "fa fa-circle" blank
                link "Results"
        , el "li" $ do
            elClass "i" "fa-li fa fa-rocket" blank
            linkFor Rankings $ do
                elClass "i" "fa fa-circle" blank
                link "Rankings"
        , el "li" $ do
            elClass "i" "fa-li fa fa-user" blank
            linkFor UserHome $ link "Xanax"
        ]
    return $ leftmost [home, others]
    where
        linkFor p t = (p <$) . _link_clicked <$> t
