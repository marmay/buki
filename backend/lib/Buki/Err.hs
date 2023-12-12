{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Super simplistic checked exceptions that I may replace with a off-the-shelf
-- library later on.
module Buki.Err (module Buki.Union, Err (..), mkFailure, mkSuccess, In (..), Embedable (..), embeddingErrors, processWith, mapWith, guardWith, mapErrors, constMapErrors, wrapErrors, mapWithPure, liftS, liftE, liftF, liftR, liftG, unwrap, unwrapM, unwrap', unwrapM', forceUnwrapOne, errToMaybe) where

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

errToMaybe :: Err errs a -> Maybe a
errToMaybe (Success a) = Just a
errToMaybe _ = Nothing

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

liftS :: forall (errs' :: [Type]) (m :: Type -> Type) (errs :: [Type]) (a :: Type) (b :: Type).
  (Monad m, Embedable errs errs') =>
  (a -> m (Err errs' b)) -> Err errs a -> m (Err errs' b)
liftS f (Success a) = f a
liftS _ (Failure e) = pure $ Failure (embed e)

liftG :: forall (errs' :: [Type]) (m :: Type -> Type) (err :: Type) (errs :: [Type]) (a :: Type).
  (Monad m, Embedable errs errs', err `In` errs') =>
  (a -> m (Maybe err)) -> Err errs a -> m (Err errs' a)
liftG f (Success a) = do
  e <- f a
  case e of
    Nothing -> pure $ Success a
    Just err -> pure $ Failure (inject err)
liftG _ (Failure e) = pure $ Failure (embed e)

liftE :: forall (errs' :: [Type]) (m :: Type -> Type) (err :: Type) (err' :: Type) (errs :: [Type]) (a :: Type).
   (Monad m, errs `Embedable` errs', err' `In` errs') =>
   (err -> m (Either err' a)) -> Err (err ': errs) a -> m (Err errs' a)
liftE _ (Success a) = pure $ Success a
liftE f (Failure e) = case project' @err @errs e of
  Left (es :: Union errs) -> pure $ Failure (embed es :: Union errs')
  Right (e' :: err) -> do
    e'' <- f e'
    case e'' of
      Left e''' -> pure $ Failure (inject e''')
      Right b -> pure $ mkSuccess b

-- liftH :: forall (m :: Type -> Type) (err :: Type) (err' :: Type) (errs :: [Type]) (a :: Type).
--   (Monad m) =>
--   (err -> m err') -> Err (err ': errs) a -> m (Err (errs `PlusPlus` err') a)
-- liftH _ (Success a) = pure $ Success a
-- liftH f (Failure e) = case project' @err @errs e of
--   Left (es :: Union errs) -> pure $ Failure es
--   Right (e' :: err) -> do
--     b <- f e'
--     pure $ mkFailure b

liftF :: forall (m :: Type -> Type) (err :: Type) (errs :: [Type]) (a :: Type).
  (Monad m) =>
  (err -> m a) -> Err (err ': errs) a -> m (Err errs a)
liftF _ (Success a) = pure $ Success a
liftF f (Failure e) = case project' @err @errs e of
  Left (es :: Union errs) -> pure $ Failure es
  Right (e' :: err) -> do
    b <- f e'
    pure $ mkSuccess b

liftR :: forall (errs' :: [Type]) (m :: Type -> Type) (errs :: [Type]) (a :: Type).
  (Monad m, errs `Embedable` errs') =>
  Err errs a -> m (Err errs' a)
liftR (Success a) = pure $ Success a
liftR (Failure e) = pure $ Failure (embed e :: Union errs')

liftErr :: forall a. a -> Err '[] a
liftErr = mkSuccess

unwrap :: forall a. Err '[] a -> a
unwrap (Success a) = a
unwrap (Failure _) = error "unwrap: Failure"

unwrapM :: forall (m :: Type -> Type) (a :: Type). (Monad m) => Err '[] a -> m a
unwrapM (Success a) = pure a
unwrapM (Failure _) = error "unwrapM: Failure"

unwrap' :: forall errs a. String -> Err errs a -> a
unwrap' _   (Success a) = a
unwrap' msg (Failure _) = error $ "unwrapping failed: " <> msg

unwrapM' :: forall (m :: Type -> Type) (errs :: [Type]) (a :: Type). (Monad m) => String -> Err errs a -> m a
unwrapM' _   (Success a) = pure a
unwrapM' msg (Failure _) = error $ "unwrapping failed: " <> msg

forceUnwrapOne :: forall err errs a. String -> Err (err ': errs) a -> Err errs a
forceUnwrapOne _   (Success a) = Success a
forceUnwrapOne msg (Failure e) = case project' @err @errs e of
  Left (es :: Union errs) -> Failure es
  Right _ -> error $ "unwrapping failed: " <> msg
