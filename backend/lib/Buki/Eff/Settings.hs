module Buki.Eff.Settings (Settings, getSettings, runSettings) where

import Effectful
import Effectful.Dispatch.Dynamic

-- | Some parts of the backend rely on particular settings to complete there task.
--   This effect provides a way to access those settings.
data Settings s :: Effect where
    GetSettings :: Settings s m s

type instance DispatchOf (Settings s) = Dynamic

getSettings :: (HasCallStack, Settings s :> es) => Eff es s
getSettings = send GetSettings

runSettings :: s -> Eff (Settings s : es) a -> Eff es a
runSettings settings = interpret $ \_ -> \case
    GetSettings -> pure settings
