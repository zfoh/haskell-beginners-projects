{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (AsyncException, fromException,
                                          throwIO, try)
import           Control.Monad           (forever, unless)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, evalStateT, state)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.HashSet            as HS
import qualified Data.Text               as T
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)
import           System.IO               (hPutStrLn, stderr)
import           WebWatch.GetLinks
import qualified WebWatch.Slack          as Slack

data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: !T.Text
    , cInterval        :: !Int
    , cSlackWebhookUrl :: !T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = do
    cPatterns        <- C.require conf "patterns"
    cUrl             <- C.require conf "url"
    cInterval        <- C.require conf "interval"
    cSlackWebhookUrl <- C.require conf "slack.webhook_url"
    return Config {..}

type LinkSet = HS.HashSet T.Text

addLinks :: [Link] -> LinkSet -> ([Link], LinkSet)
addLinks links set =
    (new, HS.union set (HS.fromList $ map lHref new))
  where
    new = filter (\l -> not $ lHref l `HS.member` set) links

type WebWatchM = ReaderT Config (StateT LinkSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg

catchExceptions :: a -> IO a -> WebWatchM a
catchExceptions def action = do
    errOrX <- liftIO $ try action
    case errOrX of
        Right x -> return x
        Left se -> case fromException se of
            Just x  -> liftIO $ throwIO (x :: AsyncException)
            Nothing -> do
                slog $ "Error: " ++ show se
                return def

watchOnce :: WebWatchM ()
watchOnce = do
    Config {..} <- ask
    slog $ "Getting links from " ++ T.unpack cUrl
    links    <- catchExceptions [] $ getMatchingLinks cPatterns cUrl
    newLinks <- state (addLinks links)
    slog $ "Got " ++ show (length links) ++ " links, " ++
        show (length newLinks) ++ " new"

    unless (null newLinks) $ do
        slog $ "Sending slack message..."
        catchExceptions () $ Slack.sendLinks cSlackWebhookUrl newLinks

    slog $ "Sleeping " ++ show cInterval ++ " minute(s)"
    liftIO $ threadDelay (cInterval * 60 * 1000 * 1000)

webWatch :: Config -> IO ()
webWatch config =
    evalStateT (runReaderT (forever watchOnce) config) HS.empty

main :: IO ()
main = do
    args <- getArgs
    case args of
        [confPath] -> do
            config <- parseConfig =<< C.load [C.Required confPath]
            webWatch config
        _ -> do
            putStrLn "Usage: webwatch CONFIG"
            exitFailure
