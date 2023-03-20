module Buki.Model.Tables (
  placeTable,
  bookTable,
  bookCopyTable,
  userTable,
  kidsgroupTable,
  userKidsgroupLinkTable,
  loanTable,
  loanRangeTable,
  tagTable,
  bookTagLinkTable,
  userBookFavoriteTable,
  sessionTable,
) where

import Buki.Model.Types
import Buki.Model.TH

makeDbTable "places" ''Place
makeDbTable "books" ''Book
makeDbTable "book_copies" ''BookCopy
makeDbTable "users" ''User
makeDbTable "kidsgroups" ''Kidsgroup
makeDbTable "user_kidsgroups" ''UserKidsgroupLink
makeDbTable "loans" ''Loan
makeDbTable "loan_ranges" ''LoanRange
makeDbTable "tags" ''Tag
makeDbTable "book_tags" ''BookTagLink
makeDbTable "user_book_favorites" ''UserBookFavorite
makeDbTable "sessions" ''Session
