module Buki.Model.Types.Loan
  ( Loan'(..)
  , Loan
  , LoanId
  , LoanField
  , pLoan
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Time.Calendar (Day)

import Buki.Model.SqlType (SqlType)
import Buki.Model.Types.BookCopy
import Buki.Model.Types.Id
import Buki.Model.Types.LoanState
import Buki.Model.Types.User
import Buki.Model.TH

data Loan' t1 t2 t3 t4 t5 t6 = Loan
  { loan'Id :: t1
  -- ^ Primary key

  , loan'UserId :: t2
  -- ^ Which user loans the book.

  , loan'BookCopyId :: t3
  -- ^ Which book copy is loaned.

  , loan'FromDay :: t4
  -- ^ When the book is loaned.

  , loan'ToDay :: t5
  -- ^ When the book is due to be returned.

  , loan'State :: t6
  -- ^ The state of the loan; the user initially reserves it, then an
  -- administrator hands it out and takes it back. There is also the
  -- possibility that either the user or an admin cancels the loan.
  } deriving (Eq, Show)
type LoanId = Id Loan'
makeDbAliases ''Loan' [ [t|LoanId|]
                      , [t|UserId|]
                      , [t|BookCopyId|]
                      , [t|Day|]
                      , [t|Day|]
                      , [t|LoanState|]
                      ]
$(makeAdaptorAndInstance' ''Loan')
