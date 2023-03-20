module Buki.Backend.Kidsgroup where

import Control.Lens

import Data.Text (Text)
import Data.UUID (UUID)

import Effectful
import Effectful.TH
import Effectful.Dispatch.Dynamic

import Buki.Err
import Buki.Eff.Db
import  Buki.Backend.Auth

import Opaleye.Operators
import qualified Opaleye as O
import qualified Buki.Model.Tables as M
import qualified Buki.Model.Types as M
import Buki.Types

data ListKidsgroup = ListKidsgroup
  { listKidsgroupId :: M.Id M.Kidsgroup'
  , listKidsgroupName :: Name
  } deriving (Show, Eq)

newtype InvalidKidsgroupIdError = InvalidKidsgroupIdError UUID
  deriving (Show, Eq)
newtype KidsgroupNotEmptyError = KidsgroupNotEmptyError [M.Id M.User']
  deriving (Show, Eq)
newtype KidsgroupAlreadyExistsError = KidsgroupAlreadyExistsError (M.Id M.Kidsgroup')
  deriving (Show, Eq)

data Kidsgroup :: Effect where
  -- | List all kidsgroups.
  ListKidsgroups :: Kidsgroup m [ListKidsgroup]

  -- | Create a new kidsgroup from its name.
  -- Returns the id of the created kidsgroup.
  -- If a kidsgroup with the same name already exists, returns an error
  -- with the id of the existing kidsgroup.
  CreateKidsgroup :: forall auth m. auth `HasPermissions` '[ 'UserManagement ] =>
    auth -> Name -> Kidsgroup m (Err '[KidsgroupAlreadyExistsError] (M.Id M.Kidsgroup'))

  -- | Return ids of all users in a kidsgroup.
  UsersInKidsgroup :: forall auth m. auth `HasPermissions` '[ 'UserManagement ] =>
    auth -> UUID -> Kidsgroup m (Err '[InvalidKidsgroupIdError] [M.Id M.User'])

  -- | Change the name of a kidsgroup.
  RenameKidsgroup :: forall auth m. auth `HasPermissions` '[ 'UserManagement ] =>
    auth -> UUID -> Text -> Kidsgroup m (Err '[InvalidKidsgroupIdError, KidsgroupAlreadyExistsError] ())

  -- | Remove a kidsgroup if it is empty.
  -- Otherwise, return an error with the ids of the users in the kidsgroup.
  RemoveEmptyKidsgroup :: forall auth m. auth `HasPermissions` '[ 'UserManagement ] =>
    auth -> UUID -> Kidsgroup m (Err '[InvalidKidsgroupIdError, KidsgroupNotEmptyError] ())

makeEffect ''Kidsgroup
type instance DispatchOf Kidsgroup = 'Dynamic

runKidsgroupDb :: (HasCallStack, Db :> es) => Eff (Kidsgroup ': es) a -> Eff es a
runKidsgroupDb = interpret $ \_ -> \case
  CreateKidsgroup _ name -> createKidsgroupDb name
  ListKidsgroups -> listKidsgroupsDb
  UsersInKidsgroup _ kidsgroupId -> usersInKidsgroupDb kidsgroupId
  RenameKidsgroup _ kidsgroupId name -> renameKidsgroupDb kidsgroupId name
  RemoveEmptyKidsgroup _ kidsgroupId -> removeEmptyKidsgroupDb kidsgroupId

usersInKidsgroupDb :: forall es. (Db :> es) => UUID -> Eff es (Err '[InvalidKidsgroupIdError] [M.Id M.User'])
usersInKidsgroupDb kidsgroupId = do
  userIds <- dbSelect $ do
    users <- O.selectTable M.userTable
    O.where_ $ (users ^. M.id) .== O.toFields kidsgroupId
    pure $ users ^. M.id
  pure $ mkSuccess userIds

listKidsgroupsDb :: forall es. (Db :> es)
  => Eff es [ListKidsgroup]
listKidsgroupsDb = do
  kidsgroups <- dbSelect @M.Kidsgroup $ O.selectTable M.kidsgroupTable
  pure $ map (\kidsgroup -> ListKidsgroup (kidsgroup ^. M.id) (kidsgroup ^. M.name)) kidsgroups

createKidsgroupDb :: forall es. (Db :> es)
  => Name -> Eff es (Err '[KidsgroupAlreadyExistsError] (M.Id M.Kidsgroup'))
createKidsgroupDb name = do
  uuid <- dbMkUuid
  kidsgroupIds <- dbInsert $ O.Insert{
    O.iTable = M.kidsgroupTable,
    O.iRows = [M.Kidsgroup (O.toFields uuid) (O.toFields name)],
    O.iReturning = O.rReturning (^. M.id),
    O.iOnConflict = Just O.DoNothing
    }
  case kidsgroupIds of
    [insertedUuid] -> pure $ mkSuccess insertedUuid
    _ -> do
      existingId <- dbSelect $ do
        kidsgroups <- O.selectTable M.kidsgroupTable
        O.where_ $ (kidsgroups ^. M.name) .== O.toFields name
        pure $ kidsgroups ^. M.id
      case existingId of
        [existingId'] -> pure $ mkFailure $ KidsgroupAlreadyExistsError existingId'
        _             -> error "createKidsgroupDb: unexpected failure; could not insert kidsgroup"

renameKidsgroupDb :: forall es. (Db :> es)
  => UUID -> Text -> Eff es (Err '[InvalidKidsgroupIdError, KidsgroupAlreadyExistsError] ())
renameKidsgroupDb kidsgroupId name = do
  _ <- dbUpdate $ O.Update{
    O.uTable = M.kidsgroupTable,
    O.uUpdateWith = O.updateEasy $ \kidsgroup -> kidsgroup & M.name .~ O.toFields name,
    O.uWhere = \kidsgroup -> (kidsgroup ^. M.id) .== O.toFields kidsgroupId,
    O.uReturning = O.rCount
    }
  pure $ mkSuccess ()

removeEmptyKidsgroupDb :: forall es. (Db :> es)
  => UUID -> Eff es (Err '[InvalidKidsgroupIdError, KidsgroupNotEmptyError] ())
removeEmptyKidsgroupDb kidsgroupId = do
  _ <- usersInKidsgroupDb kidsgroupId
    `mapWithPure` id
    `guardWith`   validateIsEmpty
    `processWith` deleteUsers
  pure $ mkSuccess ()
  where
    validateIsEmpty :: [M.UserId] -> Eff es (Maybe KidsgroupNotEmptyError)
    validateIsEmpty users = do
      case users of
        [] -> pure Nothing
        _  -> pure $ Just $ KidsgroupNotEmptyError users

    deleteUsers :: [M.UserId] -> Eff es (Err '[InvalidKidsgroupIdError, KidsgroupNotEmptyError] ())
    deleteUsers _ = do
      _ <- dbDelete $ O.Delete{
        O.dTable = M.kidsgroupTable,
        O.dWhere = \kidsgroup -> (kidsgroup ^. M.id) .== O.toFields kidsgroupId,
        O.dReturning = O.rCount
        }
      pure $ mkSuccess ()
