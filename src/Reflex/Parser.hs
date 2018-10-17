{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
module Reflex.Parser where

import Reflex
import Control.Applicative (Alternative(..))
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.List (uncons)
import Data.Maybe (isNothing)

data Result t a
  = Parsed a (Dynamic t (Derivs t))
  | ParseError

data Derivs t
  = Derivs
  { dvAdd :: Dynamic t (Result t Int)
  , dvMult :: Dynamic t (Result t Int)
  , dvPrimary :: Dynamic t (Result t Int)
  , dvDecimal :: Dynamic t (Result t Int)
  , dvSome :: forall a. (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
  , dvMany :: forall a. (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
  , dvSatisfy :: (Char -> Bool) -> Dynamic t (Result t Char)
  }

pAdd :: Reflex t => Derivs t -> Dynamic t (Result t Int)
pAdd d = do
  r1 <- dvm
  case r1 of
    Parsed n d' -> do
      dd' <- d'
      r2 <- dvSatisfy dd' (=='+')
      case r2 of
        Parsed _ d'' -> do
          dd'' <- d''
          r3 <- dvAdd dd''
          case r3 of
            Parsed m d''' -> pure $ Parsed (n+m) d'''
            ParseError -> pure ParseError
        ParseError -> dvm
    ParseError -> pure ParseError
  where
    dvm = dvMult d

pMult :: Reflex t => Derivs t -> Dynamic t (Result t Int)
pMult d = do
  r1 <- dvm
  case r1 of
    Parsed n d' -> do
      dd' <- d'
      r2 <- dvSatisfy dd' (=='*')
      case r2 of
        Parsed _ d'' -> do
          dd'' <- d''
          r3 <- dvMult dd''
          case r3 of
            Parsed m d''' -> pure $ Parsed (n*m) d'''
            ParseError -> pure ParseError
        ParseError -> dvm
    ParseError -> pure ParseError
  where
    dvm = dvPrimary d

pPrimary :: Reflex t => Derivs t -> Dynamic t (Result t Int)
pPrimary d = do
  r1 <- dvSatisfy d (=='(')
  case r1 of
    Parsed n d' -> do
      dd' <- d'
      r2 <- dvAdd dd'
      case r2 of
        Parsed n d'' -> do
          dd'' <- d''
          r3 <- dvSatisfy dd'' (==')')
          case r3 of
            Parsed _ d''' -> pure $ Parsed n d'''
            ParseError -> pure ParseError
        ParseError -> pure ParseError
    ParseError -> dvDecimal d

pMany :: Reflex t => Derivs t -> (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
pMany d r = do
  r1 <- dvs
  case r1 of
    Parsed as d' -> dvs
    ParseError -> pure $ Parsed [] (pure d)
  where
    dvs = dvSome d r

pSome :: Reflex t => Derivs t -> (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
pSome d r = do
  r1 <- r d
  case r1 of
    Parsed a d' -> do
      dd' <- d'
      r2 <- dvMany dd' r
      case r2 of
        Parsed as d'' -> pure $ Parsed (a:as) d''
        ParseError -> pure ParseError
    ParseError -> pure ParseError

pDecimal :: Reflex t => Derivs t -> Dynamic t (Result t Int)
pDecimal d = do
  r1 <- dvSome d (flip dvSatisfy isDigit)
  case r1 of
    Parsed as d' -> pure $ Parsed (read as) d'
    ParseError -> pure ParseError

data Replace
  = Replace
  { patchValue :: String
  , patchStart :: Int
  , patchSpan :: Int
  } deriving (Show, Read)

unitReplace :: Replace
unitReplace = Replace "" 0 0

runReplace :: Replace -> String -> String
runReplace (Replace s start span) = go s start span
  where
    go s start span s' =
      if start <= 0
      then
        if span <= 0
        then s <> s'
        else case s' of
          [] -> s
          x:xs -> go s start (span-1) xs
      else case s' of
        [] -> s
        x:xs -> x : go s (start-1) span xs

data DString t
  = Empty
  | Cons Char (Dynamic t (DString t))

fromString :: Reflex t => String -> DString t
fromString [] = Empty
fromString (x:xs) = Cons x . pure $ fromString xs

fromStringApp :: Reflex t => String -> DString t -> DString t
fromStringApp s ds = go s
  where
    go [] = ds
    go (x:xs) = Cons x . pure $ go xs

runReplaceD :: Reflex t => Replace -> Dynamic t (DString t) -> Dynamic t (DString t)
runReplaceD (Replace s start span) = go s start span
  where
    go :: Reflex t => String -> Int -> Int -> Dynamic t (DString t) -> Dynamic t (DString t)
    go s start span s' =
      if start <= 0
      then
        if span <= 0
        then fromStringApp s <$> s'
        else s' >>= \case
          Empty -> pure $ fromString s
          Cons x xs -> go s start (span-1) xs
      else
        (\case
            Empty -> fromString s
            Cons x xs -> Cons x $ go s (start-1) span xs) <$>
        s'

makeDString :: (Reflex t, MonadHold t m, MonadFix m) => Event t Replace -> m (Dynamic t (DString t))
makeDString eReplace = join <$> foldDyn runReplaceD (pure Empty) eReplace

toString :: Reflex t => DString t -> Dynamic t String
toString Empty = pure []
toString (Cons c cs) = (c :) <$> (cs >>= toString)

parse :: forall t m. Reflex t => Dynamic t (DString t) -> Derivs t
parse dString = d
  where
    d = Derivs{..}

    dvAdd = pAdd d
    dvMult = pMult d
    dvPrimary = pPrimary d

    dvMany :: forall a. (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
    dvMany = pMany d

    dvSome :: forall a. (Derivs t -> Dynamic t (Result t a)) -> Dynamic t (Result t [a])
    dvSome = pSome d

    dvDecimal = pDecimal d

    dvSatisfy p = go <$> dString
      where
        go :: DString t -> Result t Char
        go (Cons c cs) | p c = Parsed c . pure $ parse cs
        go _ = ParseError

fromResult :: Result t a -> Maybe a
fromResult (Parsed a _) = Just a
fromResult ParseError = Nothing
