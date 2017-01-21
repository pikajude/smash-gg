{-# LANGUAGE TemplateHaskell #-}

module Head where

import Control.Monad
import Data.FileEmbed
import Head.TH
import Language.Haskell.TH.Syntax
import System.Directory

css = $(bsToExp =<< loadCss)
