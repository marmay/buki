module Buki.TestUtil.Err where

import Buki.Err
import Effectful
import Data.Proxy (Proxy)
import Data.Kind (Type)

assertSuccess :: forall errs a es. IOE :> es => Err errs a -> Eff es a
assertSuccess (Success a) = pure a
assertSuccess (Failure _) = error "Expected success, but got error!"

assertError :: forall err errs a es. (IOE :> es, err `In` errs, Eq err) => err -> Err errs a -> Eff es ()
assertError err (Failure e) = case project @err e of
  (Just err') -> if err == err'
                 then pure ()
                 else liftIO $ error "Expected error, but got another one!"
  _ -> liftIO $ error "Expected error, but got another one!"
assertError _ (Success _) = liftIO $ error "Expected error, but got success!"

assertErrorType :: forall (err :: Type) (errs :: [Type]) a es. (IOE :> es, err `In` errs) => Proxy err -> Err errs a -> Eff es ()
assertErrorType _ (Failure e) = case project @err e of
  (Just _) -> pure ()
  _ -> liftIO $ error "Expected error, but got another one!"
assertErrorType _ (Success _) = liftIO $ error "Expected error, but got success!"
