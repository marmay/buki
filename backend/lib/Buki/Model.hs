{-# LANGUAGE OverloadedRecordDot #-}

module Buki.Model
  -- Reexports:
  ( module Buki.Model.Book
  , module Buki.Model.BookCopy
  , module Buki.Model.BookTagLink
  , module Buki.Model.Id
  , module Buki.Model.Kidsgroup
  , module Buki.Model.Loan
  , module Buki.Model.LoanRange
  , module Buki.Model.LoanState
  , module Buki.Model.Permissions
  , module Buki.Model.Place
  , module Buki.Model.Session
  , module Buki.Model.Tag
  , module Buki.Model.User
  , module Buki.Model.UserBookFavorite
  , module Buki.Model.UserKidsgroupLink

  -- Lenses that are defined by this module:
  , id
  , title
  , subTitle
  , recommended
  , isbn
  , cover
  , cachedCover
  , blurb
  , author
  , placeId
  , kidsymbol
  , loanable
  , catalogId
  , bookId
  , tagId
  , name
  , userId
  , toDay
  , fromDay
  , state
  , bookCopyId
  , permissions
  , passwordHash
  , lockedAt
  , failedLoginAttempts
  , email
  , favoriteSince
  , kidsgroupId
  , kidSymbol
  , expiresAt

  -- Some selected lens type classes:
  , HasId
  ) where

import Buki.Model.Book
import Buki.Model.BookCopy
import Buki.Model.BookTagLink
import Buki.Model.Id
import Buki.Model.Kidsgroup
import Buki.Model.Loan
import Buki.Model.LoanRange
import Buki.Model.LoanState
import Buki.Model.Permissions
import Buki.Model.Place
import Buki.Model.Session
import Buki.Model.Tag
import Buki.Model.User
import Buki.Model.UserBookFavorite
import Buki.Model.UserKidsgroupLink

import Prelude hiding (id)
import Control.Lens (makeFields)

$(makeFields ''Book')
$(makeFields ''BookCopy')
$(makeFields ''BookTagLink')
$(makeFields ''Kidsgroup')
$(makeFields ''Loan')
$(makeFields ''LoanRange')
$(makeFields ''Place')
$(makeFields ''Session')
$(makeFields ''Tag')
$(makeFields ''User')
$(makeFields ''UserBookFavorite')
$(makeFields ''UserKidsgroupLink')
