module Widgets.Navigation where

import API
import Reflex.Dom hiding (Home)

navbar = divClass "drawer" $
    elClass "ul" "menu vertical" $ leftmost <$> sequence
        [ el "li" $ linkFor Home "smash.gg"
        , el "li" $ linkFor Tournaments "Tournaments"
        , el "li" $ linkFor Results "Results"
        , el "li" $ linkFor Rankings "Rankings"
        , el "li" $ linkFor UserHome "Xanax"
        ]
    where
        linkFor p t = (p <$) . _link_clicked <$> link t
