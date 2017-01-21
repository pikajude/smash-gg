{-# LANGUAGE TemplateHaskell #-}

module Head.TH where

import           Control.Monad
import qualified Data.ByteString            as B
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.Exit
import           System.Process.ByteString

loadCss :: Q B.ByteString
loadCss = do
    files <- qRunIO $ listDirectory "css"
    forM_ files (qAddDependentFile . ("css/" <>))
    qRunIO $ do
        a <- B.readFile "css/all.scss"
        (exit, stdout, stderr) <- readProcessWithExitCode "sass" args a
        case exit of
            ExitSuccess   -> cssnano stdout
            ExitFailure _ -> error $ T.unpack $ decodeUtf8 stderr
    where
        args = [ "--scss"
               , "-Icss"
               , "-Ibower_components/foundation-sites/scss"
               , "-Ibower_components/font-awesome/scss"
               ]

cssnano :: B.ByteString -> IO B.ByteString
cssnano input = do
    (exit, stdout, stderr) <- readProcessWithExitCode "cssnano" [] input
    case exit of
        ExitSuccess   -> return stdout
        ExitFailure _ -> error $ T.unpack $ decodeUtf8 stderr
