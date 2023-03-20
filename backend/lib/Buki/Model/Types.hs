{-# LANGUAGE OverloadedRecordDot #-}

module Buki.Model.Types
  -- Reexports:
  ( module Buki.Model.Types.Book
  , module Buki.Model.Types.BookCopy
  , module Buki.Model.Types.BookTagLink
  , module Buki.Model.Types.Id
  , module Buki.Model.Types.Kidsgroup
  , module Buki.Model.Types.Loan
  , module Buki.Model.Types.LoanRange
  , module Buki.Model.Types.LoanState
  , module Buki.Model.Types.Permissions
  , module Buki.Model.Types.Place
  , module Buki.Model.Types.Session
  , module Buki.Model.Types.Tag
  , module Buki.Model.Types.User
  , module Buki.Model.Types.UserBookFavorite
  , module Buki.Model.Types.UserKidsgroupLink

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
  ) where

import Buki.Model.Types.Book
import Buki.Model.Types.BookCopy
import Buki.Model.Types.BookTagLink
import Buki.Model.Types.Id
import Buki.Model.Types.Kidsgroup
import Buki.Model.Types.Loan
import Buki.Model.Types.LoanRange
import Buki.Model.Types.LoanState
import Buki.Model.Types.Permissions
import Buki.Model.Types.Place
import Buki.Model.Types.Session
import Buki.Model.Types.Tag
import Buki.Model.Types.User
import Buki.Model.Types.UserBookFavorite
import Buki.Model.Types.UserKidsgroupLink

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
