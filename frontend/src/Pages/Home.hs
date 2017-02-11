{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Home where

import           API                    hiding (Event)
import qualified API                    (Event)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Monoid
import qualified Data.Text              as T
import           GHCJS.DOM.Types        (IsElement)
import           GHCJS.DOM.Utils
import           Reflex.Dom

banner _ = do
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
        tourneyList = [g4, beast7, g4, beast7]

carouselEntry :: forall a m t .
              (PostBuild t m, DomBuilder t m, MonadFix m, Show a, Num a, MonadHold t m)
              => Dynamic t (Int, a) -> (Int, Tournament) -> m (Event t API.Request)
carouselEntry sizeDyn (ix1, Tournament{..}) = elDynAttr "div" itemAttrs $ divClass "row" $ do
    divClass "columns large-7" $ elAttr "img" ("src" =: tournamentPreview) $ return ()

    divClass "columns large-5" $ do
        tourneyLink <- el "h3" $ link tournamentName

        attendLink <- divClass "tournament-info" $ do
            aLink <- elClass "ul" "fa-ul" $ do
                el "li" $ do
                    elClass "i" "fa-li fa fa-calendar" blank
                    text tournamentDate
                forM_ tournamentLocation $ \ l -> el "li" $ do
                    elClass "i" "fa-li fa fa-map-marker" blank
                    text l
                -- l1 <- el "li" $ do
                --     let (shown, unshown) = splitAt 4 events
                --     elClass "i" "fa-li fa fa-gamepad" blank
                --     text $ T.intercalate ", " shown
                --     el "br" blank
                --     if null unshown
                --         then return never
                --         else fmap _link_clicked $ link $ T.pack $ "show " ++ show (length unshown) ++ " more events"
                l2 <- el "li" $ do
                    elClass "i" "fa-li fa fa-users" blank
                    link $ T.pack $ show tournamentAttendees ++ " Attendees"

                return $ leftmost [ {- Events slug <$ l1
                                  , -} Attendees tournamentSlug <$ _link_clicked l2
                                  ]

            let (btn1, btn2) = if tournamentStarted
                                   then ("Brackets", "Shop")
                                   else ("Register", "Compendium")

            elClass "a" "button large" $ text btn1
            text " "
            rec (ladderLink, _) <- elDynClass' "a" classDyn $ text btn2
                let on = domEvent Mouseover ladderLink :: Event t ()
                    off = domEvent Mouseout ladderLink :: Event t ()
                isHollow <- holdDyn True $ leftmost [False <$ on, True <$ off]
                let classDyn = ffor isHollow $ \ b -> "button large" <> bool "" " hollow" b

            return aLink

        return $ leftmost [ T tournamentSlug <$ _link_clicked tourneyLink, attendLink ]
    where
        itemAttrs = ffor sizeDyn $
            \ (i, w) -> ("class" =: ("carousel-item" <> bool " hidden" "" (i == ix1))
                      <> "style" =: T.pack ("width: " ++ show (w - 120) ++ "px"))

page (HomeR ts) = do
    (divEl, e) <- banner ts
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
