module Buki.Backend.Common.Op where

import Effectful
import Buki.Eff.Db.Eff
import Buki.Eff.Db.Util
import Buki.Err
import qualified Buki.Model as M
import qualified Opaleye as O
import qualified Data.Profunctor.Product.Default as O
import Control.Lens ((^.))
import Data.Proxy (Proxy(..))

data NoSuchEntity a = NoSuchEntity
    deriving (Eq, Show)

data EntityStillReferenced a = EntityStillReferenced
    deriving (Eq, Show)

newtype EntityNameTaken a = EntityNameTaken { entityNameTakenBy :: a }
    deriving (Eq, Show)

-- deleteById :: forall a es.
--   ( Db :> es
--   , M.DbTable a
--   , M.HasId a (M.Id a)
--   , M.HasId (M.DbTableField a) (O.Field O.SqlUuid)
--   , O.Default O.FromFields (M.DbTableField a) a
--   )
--   => M.Id a -> Eff es (Err '[NoSuchEntity a, EntityStillReferenced a] a)
-- deleteById dbId = do
--   dbCatchViolation (onAnyForeignKeyViolation (mkFailure $ EntityStillReferenced @a)) $
--     dbDeleteOne (M.dbTable $ Proxy @a) (\a -> a ^. M.id O..== O.toFields dbId)
--     >>= liftE @'[NoSuchEntity a, EntityStillReferenced a] (\NoRecords -> pure $ Left $ NoSuchEntity @a)
