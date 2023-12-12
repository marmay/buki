module Buki.Eff.IsbnLookup where

import Effectful
import Effectful.TH
import Effectful.Dispatch.Dynamic

import Data.Text (Text)
import Buki.Types (Isbn)

data IsbnLookupData = IsbnLookupData
  { isbnLookupTitle :: Maybe Text
  , isbnLookupAuthor :: Maybe Text
  , isbnLookupSubTitle :: Maybe Text
  , isbnLookupBlurb :: Maybe Text
  , isbnLookupIsbn :: Maybe Isbn
  , isbnLookupCoverUrl :: Maybe Text
  } deriving (Eq, Show)

data IsbnLookup :: Effect where
  LookupIsbn :: Isbn -> IsbnLookup m (Maybe IsbnLookupData)
  
makeEffect ''IsbnLookup
type instance DispatchOf IsbnLookup = 'Dynamic

runNoIsbnLookup :: Eff (IsbnLookup ': es) a -> Eff es a
runNoIsbnLookup = interpret $ \_ -> \case
  LookupIsbn _ -> pure Nothing
