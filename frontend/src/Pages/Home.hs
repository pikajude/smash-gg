{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Home where

import           API
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Monoid
import qualified Data.Text              as T
import           GHCJS.DOM.Types        (IsElement)
import           GHCJS.DOM.Utils
import           Reflex.Dom

banner = do
    rec (b, l) <- elClass' "div" "banner" $ do
            divClass "bg" blank
            parentWidth <- fmap fst <$> elementSize b

            rec let sizeCtx = zipDyn offset parentWidth

                link' <- elDynAttr "div" (carouselAttrs sizeCtx) $
                    leftmost <$> mapM (carouselEntry sizeCtx) (zip [0..] tourneyList)

                offset <- divClass "row" $ divClass "columns large-4 large-offset-4" $
                    indicators $ length tourneyList

            return link'
    return (b, l)
    where
        carouselAttrs d = ffor d $ \ (off, width)
            -> ("class" =: "carousel clearfix"
             <> "style" =: (T.pack ("width:" ++ show ((round width - 60) * length tourneyList) ++ "px;")
                         <> T.pack ("margin-left:" ++ show (negate $ off * (round width - 60)) ++ "px;")))
        tourneyList = replicate 3 g4

carouselEntry :: forall a m t .
              (PostBuild t m, DomBuilder t m, MonadFix m, Show a, Num a, MonadHold t m)
              => Dynamic t (Int, a) -> (Int, Tournament) -> m (Event t Page)
carouselEntry sizeDyn (ix1, T{..}) = elDynAttr "div" itemAttrs $ divClass "row" $ do
    divClass "columns large-7" $ elAttr "img" ("src" =: preview) $ return ()

    divClass "columns large-5" $ do
        tourneyLink <- el "h3" $ link name

        attendLink <- divClass "tournament-info" $ do
            aLink <- elClass "ul" "fa-ul" $ do
                el "li" $ do
                    elClass "i" "fa-li fa fa-calendar" blank
                    text date
                el "li" $ do
                    elClass "i" "fa-li fa fa-map-marker" blank
                    text location
                l1 <- el "li" $ do
                    elClass "i" "fa-li fa fa-gamepad" blank
                    text $ T.intercalate ", " events
                    el "br" blank
                    link "show 10 more events"
                l2 <- el "li" $ do
                    elClass "i" "fa-li fa fa-users" blank
                    link $ T.pack $ show attendees ++ " Attendees"

                return $ leftmost [ Attendees slug <$ _link_clicked l1
                                  , Events slug <$ _link_clicked l2
                                  ]

            elClass "a" "button large" $ text "Fantasy"
            text " "
            rec (ladderLink, _) <- elDynClass' "a" classDyn $ text "Ladders"
                let on = domEvent Mouseover ladderLink :: Event t ()
                    off = domEvent Mouseout ladderLink :: Event t ()
                isHollow <- holdDyn True $ leftmost [False <$ on, True <$ off]
                let classDyn = ffor isHollow $ \ b -> "button large" <> bool "" " hollow" b

            return aLink

        return $ leftmost [ Tournament slug <$ _link_clicked tourneyLink, attendLink ]
    where
        itemAttrs = ffor sizeDyn $
            \ (i, w) -> ("class" =: ("carousel-item" <> bool " hidden" "" (i == ix1))
                      <> "style" =: T.pack ("width: " ++ show (w - 120) ++ "px"))

page = do
    (divEl, e) <- banner
    text "Hello, world"
    rec (d, _) <- elClass' "div" "test" $ display =<< elementSize d
    return e

indicators n = elClass "ol" "indicators" $ do
    rec selected <- holdDyn 1 $ leftmost evs
        evs <- forM [1..n] $ \ i -> do
            let classDyn = ffor selected $ \ i' -> if i' == i then "active" else ""
            (e, _) <- elDynClass' "li" classDyn blank
            return $ i <$ domEvent Click e
    return $ pred <$> selected
