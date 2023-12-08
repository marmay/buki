{-# LANGUAGE TemplateHaskell #-}

module Buki.Model.Types.Book (
  Book' (..),
  BookId,
  Book,
  BookField,
  pBook,
) where

import Buki.Model.SqlType
import Buki.Model.TH
import Buki.Model.Types.Id
import qualified Buki.Types as Ty
import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

-- | Represents a book; a book can have many copies, which represent
-- | physical items. All of those physical items have properties in
-- | common that are represented here.
data Book' t1 t2 t3 t4 t5 t6 t7 t8 t9 = Book
  { book'Id :: t1
  -- ^ Unique id in the database.
  , book'Title :: t2
  -- ^ Title of the book; must always be set.
  , book'Author :: t3
  -- ^ Author of the book.
  , book'SubTitle :: t4
  -- ^ Optional subtitle of the book.
  , book'Blurb :: t5
  -- ^ Optional short, promotional description of the book.
  , book'Isbn :: t6
  -- ^ Optional ISBN-10 or ISBN-13 code represented as text.
  , book'Recommended :: t7
  -- ^ Whether the book shall be promoted to all users.
  , book'Cover :: t8
  -- ^ Book's front cover.
  , book'CachedCover :: t9
  -- ^ Optional URL to a local or remote image of the
  }
  deriving (Eq, Show)

type BookId = Id Book'
makeDbAliases
  ''Book'
  [ [t|BookId|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  , [t|Maybe Ty.Isbn|]
  , [t|Bool|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  ]
makeAdaptorAndInstance' ''Book'
