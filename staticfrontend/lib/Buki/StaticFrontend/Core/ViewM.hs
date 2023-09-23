{-# LANGUAGE RecordWildCards #-}
module Buki.StaticFrontend.Core.ViewM where

import Data.Text (Text)
import Servant
import Control.Monad.Reader (Reader)
import Control.Monad.Reader.Class (asks)

newtype ViewContext = ViewContext
  { viewContextUrlPrefix :: Text
  }
 
mkLink :: forall endpoint api. (IsElem endpoint api, HasLink endpoint, ToHttpApiData (MkLink endpoint Link)) => Proxy api -> Proxy endpoint -> ViewM Text
mkLink api endpoint = do
  urlPrefix' <- asks viewContextUrlPrefix
  pure $ urlPrefix' <> toUrlPiece (safeLink api endpoint)

type ViewM a = Reader ViewContext a
