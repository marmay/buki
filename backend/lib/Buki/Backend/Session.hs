{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Buki.Backend.Session where

import Buki.Backend.Auth
import Buki.Backend.User
import Buki.Err
import Buki.Types

import Effectful
import Effectful.TH

import Buki.Eff.Db (Db, dbInsert1, dbMkUuid, dbUpdate, dbSelect1)
import Buki.Eff.Time (Time, getTime)

import Data.Default (Default (..))
import Data.Time (NominalDiffTime, addUTCTime, UTCTime)
import Effectful.Dispatch.Dynamic (interpret)

import Buki.Model.Tables qualified as M
import Buki.Model.Types qualified as M
import Buki.Validation (forceValidate)
import Control.Arrow
import Control.Lens ((&), (.~), (^.))
import Data.Text (Text)
import Opaleye qualified as O
import Opaleye.Operators ((.==))
import Data.Proxy (Proxy(..))

data InvalidSessionId = InvalidSessionId
  deriving (Eq, Show)
data LackOfPrivileges = LackOfPrivileges
  deriving (Eq, Show)
data SessionTimedOut = SessionTimedOut
  deriving (Eq, Show)

data Session :: Effect where
  MakeSession ::
    EmailAddress ->
    Password ->
    Session m (Err '[InvalidUserOrPasswordError] M.SessionId)
  Authorize ::
    forall (ps :: [AuthorizationPermission]) m.
    M.SessionId ->
    Session m (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Authorization ps))

makeEffect ''Session
type instance DispatchOf Session = 'Dynamic

newtype SessionSettings = SessionSettings
  { sessionLength :: NominalDiffTime
  }
  deriving (Eq, Show)

instance Default SessionSettings where
  def =
    SessionSettings
      { sessionLength = 900
      }

runSessionDb ::
  (Db :> es, Time :> es, User :> es) =>
  SessionSettings ->
  Eff (Session ': es) a ->
  Eff es a
runSessionDb SessionSettings{..} = interpret $ \_ -> \case
  MakeSession email password -> dbMakeSession email password sessionLength
  Authorize sessionId -> dbAuthorize sessionId sessionLength

dbMakeSession :: forall es. (Db :> es, Time :> es, User :> es) => EmailAddress -> Password -> NominalDiffTime -> Eff es (Err '[InvalidUserOrPasswordError] M.SessionId)
dbMakeSession email password sessionLength = do
  authenticateUser email password
    `mapWith` createSession
  where
    createSession (userId, name, permissions) = do
      uuid <- M.Id <$> dbMkUuid
      now <- getTime
      let sessionRecord =
            M.Session
            { M.session'Id = O.toFields uuid
            , M.session'UserId = O.toFields userId
            , M.session'Name = O.toFields name
            , M.session'Email = O.toFields email
            , M.session'Permissions = O.toFields permissions
            , M.session'ExpiresAt = O.toFields $ addUTCTime sessionLength now
            }
      mkSuccess <$> dbInsert1 M.sessionTable sessionRecord M.session'Id

dbAuthorize :: forall ps es. (Db :> es, Time :> es) => M.SessionId -> NominalDiffTime -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Authorization ps))
dbAuthorize sessionId sessionLength = do
  selectSession
    `guardWith` validateSession
    `processWith` updateSession
    `guardWith` validatePermissions
    `mapWith` makeAuthorization
  where
    selectSession :: Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Name, EmailAddress, M.Permissions, UTCTime))
    selectSession =
      constMapErrors InvalidSessionId <$>
        dbSelect1 (proc () -> do
          session <- O.selectTable M.sessionTable -< ()
          O.restrict -< M.session'Id session O..== O.toFields sessionId
          returnA
            -<
              ( session ^. M.name
              , session ^. M.email
              , session ^. M.permissions
              , session ^. M.expiresAt
              ))

    --validateSession :: (Text, Text, M.Permissions, UTCTime) -> Eff es (Maybe SessionTimedOut)
    validateSession (_, _, _, expiresAt) = do
      now <- getTime
      if now > expiresAt
        then pure $ Just SessionTimedOut
        else pure $ Nothing

    --updateSession :: (Text, Text, M.Permissions, UTCTime) -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] ())
    updateSession (name, emailAddress, permissions, expiresAt) = do
      _ <- dbUpdate
        O.Update
          { O.uTable = M.sessionTable
          , O.uUpdateWith = O.updateEasy $
              \session ->
                session
                  & M.expiresAt
                    .~ O.toFields (addUTCTime sessionLength expiresAt)
          , O.uWhere = \session -> session ^. M.id .== O.toFields sessionId
          , O.uReturning = O.rCount
          }
      pure $ mkSuccess (name, emailAddress, permissions)

    validatePermissions :: (Name, EmailAddress, M.Permissions, UTCTime) -> Eff es (Maybe LackOfPrivileges)
    validatePermissions (_, _, permissions, _) = do
      pure $ validatePermissions' (Proxy @ps) permissions

    validatePermissions' :: Proxy ps -> M.Permissions -> Maybe LackOfPrivileges
    validatePermissions' _ _ = Nothing

    makeAuthorization :: forall errs. (Name, EmailAddress, M.Permissions, UTCTime) -> Eff es (Err errs (Authorization ps))
    makeAuthorization (name, emailAddress, permissions, _) = do
      pure $ mkSuccess $ Authorization name emailAddress

