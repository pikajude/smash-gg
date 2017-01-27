{-# LANGUAGE OverloadedStrings #-}

module Main where

import           API
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Serialize                 as S
import           Data.Text
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets

main :: IO ()
main = run 8000 $ websocketsOr defaultConnectionOptions wsApp fallback

wsApp pc = acceptRequest pc >>= \ conn -> forever $ do
    putStrLn "Request accepted"
    req' <- S.decode <$> receiveData conn
    case req' of
        Left s -> putStrLn s
        Right req -> do
            liftIO $ print (req :: Page)
            sendBinaryData conn $ S.encode req

fallback _ resp = resp $ responseLBS status400 [] "Not a websocket request"
