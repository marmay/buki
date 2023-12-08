{-# LANGUAGE Arrows #-}

module Buki.Backend.Book where

import Effectful

import Buki.Eff.Db
import Buki.Backend.Auth
import Buki.Backend.IsbnLookup (IsbnLookup)
import qualified Buki.Model.Types as M
import qualified Buki.Model.Tables as M
import qualified Opaleye as O
import Opaleye.Operators ((.==))
import Control.Lens ((^.), (&), (?~), (<&>))
import Data.Text (Text)
import Buki.Types (Isbn(..))
import Buki.Err
import Control.Arrow (returnA)
import Control.Monad (join)
import qualified Database.PostgreSQL.Simple.Errors as S

data CoverData = CoverData

data ListBookData = ListBookData
  { listBookId :: M.BookId
  , listBookTitle :: Maybe Text
  , listBookAuthor :: Maybe Text
  , listBookIsbn :: Maybe Isbn
  }

data BookCopyPlaceData = BookCopyPlaceData
  { bookCopyPlaceId :: M.PlaceId
  , bookCopyPlaceName :: Text
  }

data BookCopyData = BookCopyData
  { bookCopyId :: M.BookCopyId
  , bookCopyBookId :: M.BookId
  , bookCopyPlace :: Maybe BookCopyPlaceData
  , bookCopyCatalogId :: Int
  , bookCopyLoanable :: Bool
  }

data NewBookCopyData = NewBookCopyData
  { newBookCopyBookId :: M.BookId
  , newBookCopyPlaceId :: Maybe M.PlaceId
  , newBookCopyCatalogId :: Int
  , newBookCopyLoanable :: Bool
  }

data BookData = BookData
  { bookId :: M.BookId
  , bookTitle :: Maybe Text
  , bookAuthor :: Maybe Text
  , bookSubTitle :: Maybe Text
  , bookBlurb :: Maybe Text
  , bookIsbn :: Maybe Isbn
  }

data UpdateBookData = UpdateBookData
  { updateBookTitle :: Maybe (Maybe Text)
  , updateBookAuthor :: Maybe (Maybe Text)
  , updateBookSubTitle :: Maybe (Maybe Text)
  , updateBookBlurb :: Maybe (Maybe Text)
  , updateBookIsbn :: Maybe (Maybe Isbn)
  }

data BookNotFound = BookNotFound
  deriving (Eq, Show)
data BookCopyNotFound = BookCopyNotFound
  deriving (Eq, Show)

listBooks :: forall es. (Db :> es) => Eff es [ListBookData]
listBooks = do
  books <- dbSelect $ proc () -> do
     O.selectTable M.bookTable -< ()
  pure $ fmap toListBook books

  where toListBook :: M.Book -> ListBookData
        toListBook book = ListBookData
          { listBookId = book ^. M.id
          , listBookTitle = book ^. M.title
          , listBookAuthor = book ^. M.author
          , listBookIsbn = book ^. M.isbn
          }

mkBook :: forall es. (Db :> es) => Eff es M.Book
mkBook = do
  bookId <- dbMkUuid
  pure $ M.Book {
      M.book'Id = M.Id bookId,
      M.book'Title = Nothing,
      M.book'Author = Nothing,
      M.book'SubTitle = Nothing,
      M.book'Blurb = Nothing,
      M.book'Isbn = Nothing,
      M.book'Recommended = False,
      M.book'Cover = Nothing,
      M.book'CachedCover = Nothing
      }

makeBookFromTitleAndAuthor :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> Text -> Text -> Eff es BookData
makeBookFromTitleAndAuthor _ title author = do
  book <- mkBook
  createOrFindBook
    (book & M.title  ?~ title
          & M.author ?~ author)
    (proc () -> do
      b <- O.selectTable M.bookTable -< ()
      O.restrict -< (b ^. M.title) `O.isJustAnd` (.== O.toFields title)
      O.restrict -< (b ^. M.author) `O.isJustAnd` (.== O.toFields author)
      returnA -< b)

makeBookFromIsbn :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> Isbn -> Eff es BookData
makeBookFromIsbn _ isbn = do
  book <- mkBook
  createOrFindBook
    (book & M.isbn ?~ isbn)
    (proc () -> do
      b <- O.selectTable M.bookTable -< ()
      O.restrict -< (b ^. M.isbn) `O.isJustAnd` (.== O.toFields isbn)
      returnA -< b)

createOrFindBook :: forall es. (Db :> es) => M.Book -> O.SelectArr () M.BookField -> Eff es BookData
createOrFindBook book query = do
  dbWithTransaction $ do
    (book' :: Err '[] M.Book) <- dbCatch (ConstraintViolationHandler handler) $ mkSuccess <$> dbInsert1 M.bookTable (O.toFields book) id
    pure $ toBookData (unwrap book')
  where
    handler :: S.ConstraintViolation -> Eff es (Maybe (Err '[] M.Book))
    handler (S.UniqueViolation _) = do
      b <- dbSelect1' query >>= unwrapM' "Your database is broken!"
      pure $ Just $ mkSuccess b
    handler _ = pure Nothing


showBook :: forall es. (Db :> es) => M.BookId -> Eff es (Err '[ BookNotFound ] BookData)
showBook bookId = do
  (fmap . fmap) toBookData $
    selectBook >>= liftE @'[ BookNotFound ] (\NoRecordsFound -> pure $ Left BookNotFound)
  where
    selectBook :: Eff es (Err '[ NoRecordsFound ] M.Book)
    selectBook = dbSelect1' $ proc () -> do
      b <- O.selectTable M.bookTable -< ()
      O.restrict -< (b ^. M.id) .== O.toFields bookId
      returnA -< b

updateBook :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> M.BookId -> UpdateBookData -> Eff es (Err '[ BookNotFound ] BookData)
updateBook _ = undefined

removeBook :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> M.BookId -> Eff es (Err '[ BookNotFound ] BookData) 
removeBook _ bookId = do
  dbWithTransaction $ do
    _ <- dbDelete O.Delete { O.dTable = M.bookCopyTable
                           , O.dWhere = \bc -> (bc ^. M.bookId) .== O.toFields bookId
                           , O.dReturning = O.rCount }
    dbDelete1 M.bookTable (\b -> (b ^. M.id) .== O.toFields bookId) id
  >>= liftE @'[ BookNotFound ] (\NoRecordsFound -> pure $ Left BookNotFound)
  <&> fmap toBookData

listBookCopies :: forall es. (Db :> es) => M.BookId -> Eff es [BookCopyData]
listBookCopies bookId = do
  bookCopiesAndPlaces <- dbSelect $ proc () -> do
    bookCopy <- O.selectTable M.bookCopyTable -< ()
    O.restrict -< (bookCopy ^. M.bookId) .== O.toFields bookId
    place <- O.optionalRestrict (O.selectTable M.placeTable)
             -< \p -> (bookCopy ^. M.placeId) `O.isJustAnd` (.== (p ^. M.id))
    returnA -< (bookCopy, place)
  pure $ fmap (uncurry toBookCopyData) bookCopiesAndPlaces

completeBookCopyData :: forall es. (Db :> es) => M.BookCopy -> Eff es BookCopyData
completeBookCopyData bookCopy = do
  (place :: Maybe (Maybe M.Place)) <- dbSelect1Maybe $ proc () -> do
    place <- O.optionalRestrict (O.selectTable M.placeTable) -<
      \p -> O.toFields (bookCopy ^. M.placeId) `O.isJustAnd` (.== (p ^. M.id))
    returnA -< place
  pure $ toBookCopyData bookCopy (join place)

addBookCopy :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> NewBookCopyData -> Eff es BookCopyData
addBookCopy _ newBookCopyData = do
  bookCopyId <- dbMkUuid
  dbInsert1 M.bookCopyTable (bookCopyData $ M.Id bookCopyId) id
    >>= completeBookCopyData
  where
    bookCopyData :: M.Id M.BookCopy -> M.BookCopyField
    bookCopyData bookCopyId = O.toFields M.BookCopy
      { M.bookCopy'Id = bookCopyId
      , M.bookCopy'BookId = newBookCopyBookId newBookCopyData
      , M.bookCopy'PlaceId = newBookCopyPlaceId newBookCopyData
      , M.bookCopy'CatalogId = newBookCopyCatalogId newBookCopyData
      , M.bookCopy'Loanable = newBookCopyLoanable newBookCopyData
      }

removeBookCopy :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement]) =>
  auth -> M.BookCopyId -> Eff es (Err '[ BookCopyNotFound ] BookCopyData)
removeBookCopy _ bookCopyId =
  dbDelete1 M.bookCopyTable (\bc -> (bc ^. M.id) .== O.toFields bookCopyId) id
  >>= liftE @'[ BookCopyNotFound ] (\NoRecordsFound -> pure $ Left BookCopyNotFound)
  >>= liftS (\b -> do
                c <- completeBookCopyData b
                pure $ mkSuccess c)

toBookCopyData :: M.BookCopy -> Maybe M.Place -> BookCopyData
toBookCopyData bookCopy place = BookCopyData
  { bookCopyId = bookCopy ^. M.id
  , bookCopyBookId = bookCopy ^. M.bookId
  , bookCopyPlace = toPlaceData <$> place
  , bookCopyCatalogId = bookCopy ^. M.catalogId
  , bookCopyLoanable = bookCopy ^. M.loanable
  }
  where toPlaceData :: M.Place -> BookCopyPlaceData
        toPlaceData p = BookCopyPlaceData
          { bookCopyPlaceId = p ^. M.id
          , bookCopyPlaceName = p ^. M.name
          }

toBookData :: M.Book -> BookData
toBookData book =
  BookData
       { bookId = book ^. M.id
       , bookTitle = book ^. M.title
       , bookAuthor = book ^. M.author
       , bookSubTitle = book ^. M.subTitle
       , bookBlurb = book ^. M.blurb
       , bookIsbn = book ^. M.isbn
       }
