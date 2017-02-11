{-# LANGUAGE OverloadedStrings #-}

module Main where

import           API
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Serialize                 as S
import           Data.Text
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

main :: IO ()
main = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "Compiling subproject"
    setSGR [Reset]
    hFlush stdout

    cwd <- getCurrentDirectory
    ph <- runProcess "cabal" ["build"] (Just $ cwd <> "/../frontend") Nothing Nothing Nothing Nothing
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $
        die "Failed to run cabal build"

    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "Running test server"
    setSGR [Reset]
    hFlush stdout

    serverThread <- forkIO $ do
        s <- getCurrentDirectory
        let set_ = defaultFileServerSettings (s <> "/../frontend/dist/build/frontend/frontend.jsexe")
            app = staticApp (set_ { ss404Handler = Just $ \ req resp ->
                                      app (req { rawPathInfo = "/", pathInfo = [] }) resp
                                  })
        run 8080 app

    run 8000 (websocketsOr defaultConnectionOptions wsApp fallback)
        `finally` killThread serverThread

wsApp pc = acceptRequest pc >>= \ conn -> forever $ do
    putStrLn "Request accepted"
    req' <- S.decode <$> receiveData conn
    case req' of
        Left s -> putStrLn s
        Right req -> do
            liftIO $ print (req :: API.Request)
            sendBinaryData conn $ S.encode $ HomeR []

fallback _ resp = resp $ responseLBS status400 [] "Not a websocket request"
