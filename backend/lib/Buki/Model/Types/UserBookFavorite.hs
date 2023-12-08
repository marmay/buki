module Buki.Model.Types.UserBookFavorite
  ( UserBookFavorite'(..)
  , UserBookFavorite
  , UserBookFavoriteId
  , UserBookFavoriteField
  , pUserBookFavorite
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Time.LocalTime (LocalTime)

import Buki.Model.SqlType (SqlType)
import Buki.Model.TH
import Buki.Model.Types.Book
import Buki.Model.Types.Id
import Buki.Model.Types.User

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
