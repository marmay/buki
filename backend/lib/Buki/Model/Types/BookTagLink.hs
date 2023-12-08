module Buki.Model.Types.BookTagLink
  ( BookTagLink'(..)
  , BookTagLink
  , BookTagLinkId
  , BookTagLinkField
  , pBookTagLink
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')

import Buki.Model.SqlType (SqlType)
import Buki.Model.TH
import Buki.Model.Types.Book
import Buki.Model.Types.Id
import Buki.Model.Types.Tag

data BookTagLink' t1 t2 t3 = BookTagLink
  { bookTagLink'Id :: t1
  -- ^ Primary key

  , bookTagLink'BookId :: t2
  -- ^ Which book is linked to the tag.

  , bookTagLink'TagId :: t3
  -- ^ Which tag is linked to the book.
  }
type BookTagLinkId = Id BookTagLink'

makeDbAliases ''BookTagLink' [ [t|BookTagLinkId|]
                             , [t|BookId|]
                             , [t|TagId|]
                             ]
$(makeAdaptorAndInstance' ''BookTagLink')
