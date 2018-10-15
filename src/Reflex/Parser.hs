{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
module Reflex.Parser where

import Reflex
import Control.Applicative (Alternative(..))
import Data.Either (isLeft)
import Data.List (uncons)
import Data.Maybe (isNothing)

data ParseError
  = Unexpected Char
  | UnexpectedEof
  deriving Show

data ParseOutput t a
  = ParseOutput
  { poResult :: Event t (String, a)
  , poError :: Event t ParseError
  }
deriving instance Reflex t => Functor (ParseOutput t)

-- | Parser t a ~ (Time -> String) -> [Time * String * a]
newtype Parser t m a
  = Parser
  { unParser :: Event t String -> m (ParseOutput t a)
  }
deriving instance (Reflex t, Functor m) => Functor (Parser t m)

runParser
  :: (Reflex t, Functor m)
  => Parser t m a
  -> Event t String
  -> m (ParseOutput t a)
runParser p eInput = unParser p eInput

zero :: (Reflex t, Applicative m) => Parser t m a
zero =
  Parser $ \eInput ->
  pure $ ParseOutput { poResult = never, poError = never }

choice
  :: (Reflex t, Applicative m)
  => Parser t m a
  -> Parser t m a
  -> Parser t m a
choice (Parser pa) (Parser pb) =
  Parser $ \eInput ->
    (\eOutput1 eOutput2 ->
       ParseOutput
       { poResult = leftmost [poResult eOutput1, poResult eOutput2]
       , poError = leftmost [poError eOutput1, poError eOutput2]
       }) <$>
    pa eInput <*>
    pb eInput

unit :: (Reflex t, Applicative m) => Parser t m ()
unit =
  Parser $ \eInput ->
  pure $ ParseOutput { poResult = (, ()) <$> eInput, poError = never }

tensor
  :: (Reflex t, MonadHold t m)
  => Parser t m a
  -> Parser t m b
  -> Parser t m (a, b)
tensor (Parser pa) (Parser pb) =
  Parser $ \eInput -> do
    eOutput1 <- pa eInput
    eOutput2 <- pb $ fst <$> poResult eOutput1
    poError <-
      switchHoldPromptly (poError eOutput1) $
      poError eOutput2 <$ poResult eOutput1
    poResult <-
      switchHoldPromptly never $
      (\a -> (\(str, b) -> (str, (a, b))) <$> poResult eOutput2) . snd <$>
      poResult eOutput1
    pure ParseOutput{..}

instance (Reflex t, MonadHold t m) => Applicative (Parser t m) where
  pure a = a <$ unit
  a <*> b = (\(f, x) -> f x) <$> tensor a b

instance (Reflex t, MonadHold t m) => Alternative (Parser t m) where
  empty = zero
  (<|>) = choice

satisfy :: (Reflex t, Applicative m) => (Char -> Bool) -> Parser t m Char
satisfy p =
  Parser $ \eInput ->
  let
    (poError, poResult) =
      fanEither $
      fmap
        (\str ->
           case uncons str of
             Nothing -> Left UnexpectedEof
             Just (x, xs) ->
               if p x
               then Right (xs, x)
               else Left $ Unexpected x)
        eInput
  in
    pure $ ParseOutput{..}

char :: (Reflex t, Applicative m) => Char -> Parser t m Char
char c = satisfy (==c)
