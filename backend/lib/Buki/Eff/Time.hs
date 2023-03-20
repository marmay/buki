module Buki.Eff.Time where

import Effectful
import Effectful.TH

import Data.Time (UTCTime, getCurrentTime)
import Effectful.Dispatch.Dynamic

data Time :: Effect where
  GetTime :: Time m UTCTime

makeEffect ''Time

runTimeFake :: UTCTime -> Eff (Time ': es) a -> Eff es a
runTimeFake time = interpret $ \_ -> \case
  GetTime -> pure time

runTime :: (IOE :> es) => Eff (Time ': es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetTime -> liftIO getCurrentTime
