{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.BookCopy
  ( BookCopy'(..)
  , BookCopy
  , BookCopyId
  , BookCopyField
  , pBookCopy
  , bookCopyTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Book
import Buki.Model.Id
import Buki.Model.Place

data BookCopy' t1 t2 t3 t4 t5 = BookCopy
  { bookCopy'Id :: t1
  -- ^ Primary key

  , bookCopy'CatalogId :: t2
  -- ^ Unique id of the book in the catalog.

  , bookCopy'Loanable :: t3
  -- ^ Whether the copy of the book is loanable.

  , bookCopy'BookId :: t4
  -- ^ Which book this is a copy of.

  , bookCopy'PlaceId :: t5
  -- ^ Where the copy of the book is located. If the book is lost,
  -- this field is set to 'Nothing'.
  }
type BookCopyId = Id BookCopy'
makeDbAliases ''BookCopy' [ [t|BookCopyId|]
                          , [t|Int|]
                          , [t|Bool|]
                          , [t|BookId|]
                          , [t|Maybe PlaceId|]
                          ]
$(makeAdaptorAndInstance' ''BookCopy')
$(makeDbTable "book_copies" ''BookCopy)
