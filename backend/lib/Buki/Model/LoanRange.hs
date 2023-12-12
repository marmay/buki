{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.LoanRange
  ( LoanRange'(..)
  , LoanRange
  , LoanRangeId
  , LoanRangeField
  , pLoanRange
  , loanRangeTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Time.Calendar (Day)

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Id

-- | A range of days in which a book can be loaned.
data LoanRange' t1 t2 t3 = LoanRange
  { loanRange'Id :: t1
  -- ^ Primary key

  , loanRange'FromDay :: t2
  -- ^ The day, the book is handed out.

  , loanRange'ToDay :: t3
  -- ^ The day, the book is due to be returned.
  } deriving (Eq, Show)
type LoanRangeId = Id LoanRange'
makeDbAliases ''LoanRange' [ [t|LoanRangeId|]
                           , [t|Day|]
                           , [t|Day|]
                           ]
makeAdaptorAndInstance' ''LoanRange'
makeDbTable "loan_ranges" ''LoanRange
