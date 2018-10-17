module Main where

import Reflex
import Reflex.Host.Basic
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)

import Reflex.Parser

main :: IO ()
main =
  basicHostForever $ do
    var <- liftIO $ newTVarIO unitReplace

    (eInput, triggerInput) <- newTriggerEvent
    eReplace <- performEvent $ liftIO (readTVarIO var) <$ eInput

    dString <- makeDString eReplace

    let dOutput = dvAdd $ parse dString
    performEvent $ liftIO . print <$> updated (dString >>= toString)
    performEvent $ liftIO . print <$> eReplace
    performEvent $ liftIO . print . fromResult <$> updated dOutput

    void . liftIO . forkIO $ loop var triggerInput
  where
    loop var f = do
      line <- getLine
      a <- fmap readMaybe getLine :: IO (Maybe Int)
      b <- fmap readMaybe getLine :: IO (Maybe Int)
      case Replace line <$> a <*> b of
        Nothing -> loop var f
        Just r -> do
          () <- atomically $ writeTVar var r
          () <- f ()
          loop var f
