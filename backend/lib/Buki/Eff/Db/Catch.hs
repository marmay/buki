-- | Provides types with an algebra to catch database exceptions and
--   transform them.
module Buki.Eff.Db.Catch where

import qualified Database.PostgreSQL.Simple.Errors as S

newtype ConstraintViolationHandler m a = ConstraintViolationHandler {
    runConstraintViolationHandler :: S.ConstraintViolation -> m (Maybe a)
  }

instance (Monad m) => Semigroup (ConstraintViolationHandler m a) where
  ConstraintViolationHandler a <> ConstraintViolationHandler b = ConstraintViolationHandler $ \cv -> do
    r <- a cv
    case r of
      Nothing -> b cv
      Just r' -> pure $ Just r'

instance (Monad m) => Monoid (ConstraintViolationHandler m a) where
  mempty = ConstraintViolationHandler $ \_ -> pure Nothing

onAnyUniqueViolation :: forall m a. (Monad m) => a -> ConstraintViolationHandler m a
onAnyUniqueViolation r = ConstraintViolationHandler $
  \case (S.UniqueViolation _) -> pure $ Just r
        _                     -> pure Nothing

onAnyUniqueViolationDo :: forall m a. (Monad m) => m a -> ConstraintViolationHandler m a
onAnyUniqueViolationDo r = ConstraintViolationHandler $
  \case (S.UniqueViolation _) -> Just <$> r
        _                     -> pure Nothing

onAnyForeignKeyViolation :: forall m a. (Monad m) => a -> ConstraintViolationHandler m a
onAnyForeignKeyViolation r = ConstraintViolationHandler $
  \case (S.ForeignKeyViolation _ _) -> pure $ Just r
        _                           -> pure Nothing
