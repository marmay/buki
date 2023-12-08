module Buki.Model.Types.LoanState
  ( SqlLoanState
  , LoanState(..)
  ) where

import Opaleye ( DefaultFromField(..), FromField, ToFields, Field )
import Opaleye.Internal.Inferrable ( Inferrable(Inferrable) )
import Opaleye.Experimental.Enum ( EnumMapper(..), enumMapper )
import Data.Profunctor.Product.Default (Default(..))
import Buki.Model.SqlType (SqlType)

data SqlLoanState
data LoanState
  = Reserved
  | HandedOut
  | Returned
  | Canceled
  deriving (Eq, Ord, Enum, Show)

toSqlLoanStateString :: LoanState -> String
toSqlLoanStateString p = case p of
  Reserved      -> "reserved"
  HandedOut     -> "handed_out"
  Returned      -> "returned"
  Canceled      -> "canceled"
fromSqlLoanStateString :: String -> Maybe LoanState
fromSqlLoanStateString s = case s of
  "reserved"    -> Just Reserved
  "handed_out"  -> Just HandedOut
  "returned"    -> Just Returned
  "canceled"    -> Just Canceled
  _             -> Nothing
sqlLoanStateMapper :: EnumMapper SqlLoanState LoanState
sqlLoanStateMapper = enumMapper "loan_state"
                                   fromSqlLoanStateString
                                   toSqlLoanStateString

type instance SqlType LoanState = Field SqlLoanState

instance DefaultFromField SqlLoanState LoanState where
  defaultFromField = enumFromField sqlLoanStateMapper
instance permissions ~ LoanState
  => Default (Inferrable FromField) SqlLoanState permissions where
    def = Inferrable def
instance Default ToFields LoanState (Field SqlLoanState) where
  def = enumToFields sqlLoanStateMapper
