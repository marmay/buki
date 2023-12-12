{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.BookTagLink (
    BookTagLink' (..),
    BookTagLink,
    BookTagLinkId,
    BookTagLinkField,
    pBookTagLink,
    bookTagLinkTable,
) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Book
import Buki.Model.Id
import Buki.Model.Tag

data BookTagLink' t1 t2 t3 = BookTagLink
    { bookTagLink'Id :: t1
    -- ^ Primary key
    , bookTagLink'BookId :: t2
    -- ^ Which book is linked to the tag.
    , bookTagLink'TagId :: t3
    -- ^ Which tag is linked to the book.
    }
type BookTagLinkId = Id BookTagLink'

makeDbAliases
    ''BookTagLink'
    [ [t|BookTagLinkId|]
    , [t|BookId|]
    , [t|TagId|]
    ]
$(makeAdaptorAndInstance' ''BookTagLink')
makeDbTable "book_tags" ''BookTagLink
