{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Super simplistic checked exceptions that I may replace with a off-the-shelf
-- library later on.
module Buki.Err (module Buki.Union, Err (..), mkFailure, mkSuccess, In (..), Embedable (..), embeddingErrors, processWith, mapWith, guardWith, mapErrors, constMapErrors, wrapErrors, mapWithPure) where

import Buki.Union

import Data.Kind

data Err (errs :: [Type]) a
  = Failure (Union errs)
  | Success a

deriving instance Functor (Err errs)

instance Applicative (Err errs) where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e

instance Monad (Err errs) where
  Success a >>= f = f a
  Failure e >>= _ = Failure e

mkFailure :: forall err e a. (e `In` err) => e -> Err err a
mkFailure = Failure . inject

mkSuccess :: forall err a. a -> Err err a
mkSuccess = Success

embeddingErrors ::
  forall (m :: Type -> Type) (errs' :: [Type]) (errs :: [Type]) (a :: Type) (b :: Type).
  (Monad m, Embedable errs errs') =>
  m (Err errs a) ->
  (a -> m (Err errs' b)) ->
  m (Err errs' b)
embeddingErrors action f = do
  e <- action
  case e of
    Success a -> f a
    Failure failure -> pure $ Failure (embed failure)

wrapErrors ::
  forall (wrapper :: Type -> Type) (errs :: [Type]) (a :: Type).
  (Applicative wrapper) =>
  Err errs a ->
  Err '[wrapper (Union errs)] a
wrapErrors (Success a) = mkSuccess a
wrapErrors (Failure f) = mkFailure (pure f :: wrapper (Union errs))

constMapErrors ::
  forall (err :: Type) (errs :: [Type]) (errs' :: [Type]) (a :: Type).
  (err `In` errs') =>
  err ->
  Err errs a ->
  Err errs' a
constMapErrors _ (Success a) = mkSuccess a
constMapErrors e (Failure _) = mkFailure e

mapErrors ::
  forall (errs' :: [Type]) (errs :: [Type]) (a :: Type).
  (Union errs -> Union errs') ->
  Err errs a ->
  Err errs' a
mapErrors _ (Success a) = mkSuccess a
mapErrors f (Failure e) = Failure (f e :: Union errs')

guardWith ::
  forall (m :: Type -> Type) (err :: Type) (errs :: [Type]) (a :: Type).
  (Monad m, err `In` errs) =>
  m (Err errs a) ->
  (a -> m (Maybe err)) ->
  m (Err errs a)
guardWith action h = do
  a <- action
  case a of
    (Success a') -> do
      e <- h a'
      case e of
        Nothing -> pure $ mkSuccess a'
        Just err -> pure $ mkFailure err
    (Failure e) -> pure $ Failure e

mapWith ::
  forall (m :: Type -> Type) (errs' :: [Type]) (errs :: [Type]) (a :: Type) (b :: Type).
  (Monad m, Embedable errs errs') =>
  m (Err errs a) ->
  (a -> m (Err errs' b)) ->
  m (Err errs' b)
mapWith = embeddingErrors

mapWithPure ::
  forall (m :: Type -> Type) (errs' :: [Type]) (errs :: [Type]) (a :: Type) (b :: Type).
  (Monad m, Embedable errs errs') =>
  m (Err errs a) ->
  (a -> b) ->
  m (Err errs' b)
mapWithPure action f = do
  a <- action
  case a of
    (Success a') -> pure $ mkSuccess (f a')
    (Failure e) -> pure $ Failure (embed e)

processWith ::
  forall (m :: Type -> Type) (errs :: [Type]) (a :: Type) (b :: Type).
  (Monad m) =>
  m (Err errs a) ->
  (a -> m (Err errs b)) ->
  m (Err errs a)
processWith action h = do
  a <- action
  case a of
    (Success a') -> do
      e <- h a'
      case e of
        (Success _) -> pure $ mkSuccess a'
        (Failure err) -> pure $ Failure err
    (Failure e) -> pure $ Failure e
