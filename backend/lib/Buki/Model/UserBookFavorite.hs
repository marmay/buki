{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.UserBookFavorite
  ( UserBookFavorite'(..)
  , UserBookFavorite
  , UserBookFavoriteId
  , UserBookFavoriteField
  , pUserBookFavorite
  , userBookFavoriteTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Time.LocalTime (LocalTime)

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Book
import Buki.Model.Id
import Buki.Model.User

data UserBookFavorite' t1 t2 t3 t4 = UserBookFavorite
  { userBookFavorite'Id :: t1
  , userBookFavorite'UserId :: t2
  , userBookFavorite'BookId :: t3
  , userBookFavorite'FavoriteSince :: t4
  }
type UserBookFavoriteId = Id UserBookFavorite'
makeDbAliases ''UserBookFavorite' [ [t|UserBookFavoriteId|]
                                  , [t|UserId|]
                                  , [t|BookId|]
                                  , [t|LocalTime|]
                                  ]
makeAdaptorAndInstance' ''UserBookFavorite'
makeDbTable "user_book_favorites" ''UserBookFavorite
