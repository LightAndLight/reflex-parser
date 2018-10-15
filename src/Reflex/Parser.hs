{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language TupleSections #-}
module Reflex.Parser where

import Reflex
import Control.Applicative (Alternative(..))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)

data ParseError
  = Empty
  | Unexpected Char
  | UnexpectedEof

-- | Parser t a ~ (Time -> String) -> [(Time * ParseError)] * [(Time * String * a)]
newtype Parser t m a
  = Parser
  { unParser
    :: Event t String -> m (Event t (NonEmpty ParseError), Event t (String, a))
  }
deriving instance (Reflex t, Functor m) => Functor (Parser t m)

-- | For all inputs, returns an 'Empty' error, and never yields a result
zero :: (Reflex t, Applicative m) => Parser t m a
zero = Parser $ \eInput -> pure (pure Empty <$ eInput, never)

choice :: (Reflex t, Applicative m) => Parser t m a -> Parser t m a -> Parser t m a
choice (Parser pa) (Parser pb) =
  Parser $ \eInput ->
    (\(eErr1, eRes1) (eErr2, eRes2) ->
      ( leftmost [eErr1, eErr2]
      -- If we used the LL style first- and follow- sets,
      -- we could say that they will never yield simultaneously?
      , leftmost [eRes1, eRes2]
      )) <$>
    pa eInput <*>
    pb eInput

-- | For all inputs, yield an @a@, never causing an error
unit :: (Reflex t, Applicative m)=> Parser t m ()
unit = Parser $ \eInput -> pure (never, (, ()) <$> eInput)

tensor :: (Reflex t, MonadHold t m) => Parser t m a -> Parser t m b -> Parser t m (a, b)
tensor (Parser pa) (Parser pb) =
  Parser $ \eInput -> do
    (eErr1, eRes1) <- pa eInput
    (eErr2, eRes2) <- pb $ fst <$> eRes1
    eErr3 <- switchHold eErr1 $ eErr1 <> eErr2 <$ eErr1
    eRes3 <- switchHold never $ (\(_, a) -> (\(str, b) -> (str, (a, b))) <$> eRes2) <$> eRes1
    pure (eErr3, eRes3)

instance (Reflex t, MonadHold t m) => Applicative (Parser t m) where
  pure a = a <$ unit
  a <*> b = (\(f, x) -> f x) <$> tensor a b

instance (Reflex t, MonadHold t m) => Alternative (Parser t m) where
  empty = zero
  (<|>) = choice

satisfy :: (Reflex t, Applicative m) => (Char -> Bool) -> Parser t m Char
satisfy p =
  Parser $ \eInput ->
  pure . fanEither $
  (\str -> case uncons str of
      Nothing -> Left $ pure UnexpectedEof
      Just (x, xs) ->
        if p x
        then Right (xs, x)
        else Left . pure $ Unexpected x) <$> eInput