module Pages.Results where

import Reflex.Dom

page _ = do
    divClass "columns" $ text "Results page"
    return never
