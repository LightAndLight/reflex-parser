module Main where

import Reflex
import Reflex.Host.Basic
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Reflex.Parser

data Boo = Bar | Bop deriving Show

boo :: (Reflex t, MonadHold t m) => Parser t m Boo
boo =
  char 'b' *>
  (Bar <$ char 'a' <* char 'r' <|>
   Bop <$ char 'o' <* char 'p')

main :: IO ()
main =
  basicHostForever $ do
    var <- liftIO $ newTVarIO ""

    (eInput, triggerInput) <- newTriggerEvent
    eInput' <- performEvent $ liftIO (readTVarIO var) <$ eInput

    eOutput <- runParser boo eInput'
    performEvent $ liftIO . print <$> poResult eOutput
    performEvent $ liftIO . print <$> poError eOutput

    void . liftIO . forkIO $ loop var triggerInput
  where
    loop var f = do
      line <- getLine
      () <- atomically $ writeTVar var line
      () <- f ()
      loop var f
