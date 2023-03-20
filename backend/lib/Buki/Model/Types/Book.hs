{-# LANGUAGE TemplateHaskell #-}

module Buki.Model.Types.Book (
  Book' (..),
  BookId,
  Book,
  BookField,
  pBook,
) where

import Buki.Model.TH
import Buki.Model.Types.Id
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
  , book'SubTitle :: t3
  -- ^ Optional subtitle of the book.
  , book'Blurb :: t4
  -- ^ Optional short, promotional description of the book.
  , book'Isbn :: t5
  -- ^ Optional ISBN-10 or ISBN-13 code represented as text.
  , book'Recommended :: t6
  -- ^ Whether the book shall be promoted to all users.
  , book'Cover :: t7
  -- ^ Book's front cover.
  , book'Author :: t8
  -- ^ Author of the book.
  , book'CachedCover :: t9
  -- ^ Optional URL to a local or remote image of the
  }
  deriving (Eq, Show)

type BookId = Id Book'
makeDbAliases
  ''Book'
  [ [t|BookId|]
  , [t|Text|]
  , [t|Text|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  , [t|Bool|]
  , [t|Maybe Text|]
  , [t|Maybe Text|]
  ]
makeAdaptorAndInstance' ''Book'
