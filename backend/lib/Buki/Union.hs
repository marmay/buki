{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Buki.Union where

-- This is an implementation of open unions in Haskell. It is a straight-forward
-- implementation that possibly has some performance issues and should be regarded
-- as a proof of concept for now.
--
-- In contrast to some other implementations that I could find, this one supports
-- embedding unions into other unions, as long as the type list of the latter contains
-- all the types of the former. This is useful for implementing checked exceptions,
-- where we want to accumulate exceptions from multiple sources and handle them in
-- the end in a single place.
--
-- As of now, I am not convinced the following is possible, but the general idea is
-- that we have an @Either@ like type on top of the open union type, where the left
-- side represents the error case and the right side represents the success case.

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Kind (Type, Constraint)
import Data.Typeable (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class r `In` rs where
  index :: Proxy r -> Proxy rs -> Word

instance {-# OVERLAPPABLE #-} forall (r :: Type) (rs :: [Type]). r `In` (r ': rs) where
  index _ _ = 0

instance {-# OVERLAPS #-} forall (r :: Type) (s :: Type) (rs :: [Type]). r `In` rs => r `In` (s ': rs) where
  index _ _ = 1 + index (Proxy @r) (Proxy @rs)

class rs `Embedable` ss where
  embedMap :: Proxy rs -> Proxy ss -> Map Word Word

instance forall (r :: Type) (rs :: [Type]) (ss :: [Type]). (r `In` ss, rs `Embedable` ss)
  => (r ': rs) `Embedable` ss where
  embedMap _ _ = Map.insert (index (Proxy @r) (Proxy @ss)) 0 (embedMap (Proxy @rs) (Proxy @ss))
instance forall (ss :: [Type]). '[] `Embedable` ss where
  embedMap _ _ = Map.empty

type family AllIn (rs :: [Type]) (ss :: [Type]) :: Constraint where
  AllIn '[] ss = ()
  AllIn (r ': rs) ss = (r `In` ss, AllIn rs ss, Embedable (r ': rs) ss)

data Union (rs :: [Type]) where
  Union :: !Word -> r -> Union rs

inject :: forall r rs. r `In` rs => r -> Union rs
inject = Union $ index (Proxy @r) (Proxy @rs)

project :: forall r rs. r `In` rs => Union rs -> Maybe r
project (Union i r)
  | i == index (Proxy @r) (Proxy @rs) = Just $ unsafeCoerce r
  | otherwise = Nothing

project' :: forall r rs. Union (r ': rs) -> Either (Union rs) r
project' (Union i r)
  | i == 0    = Right $ unsafeCoerce r
  | otherwise = Left $ Union (i - 1) r

embed :: forall ss rs. rs `Embedable` ss => Union rs -> Union ss
embed (Union i r) = Union (embedMap (Proxy @rs) (Proxy @ss) Map.! i) r

type family Combine (rs :: [Type]) (rs' :: [Type]) :: [Type] where
  Combine '[] rs = rs
  Combine (r ': rs) rs' = r ': Combine rs rs'

data UnionHandler (r :: Type) (rs :: [Type]) (a :: Type)  where
  UnionHandler :: forall r a. (r -> a) -> UnionHandler r '[] a
  (:::) :: UnionHandler r1 '[] a -> UnionHandler r2 rs a -> UnionHandler r1 (r2 ': rs) a

tryRunUnionHandler :: forall r rs rs' a. (r `In` rs', rs `AllIn` rs') =>
  UnionHandler r rs a -> Union rs' -> Maybe a
tryRunUnionHandler (UnionHandler f) u = f <$> project u
tryRunUnionHandler (h1 ::: h2) u = tryRunUnionHandler h1 u `orElse` tryRunUnionHandler h2 u
  where orElse :: Maybe a -> Maybe a -> Maybe a
        orElse (Just a) _ = Just a
        orElse _ b = b

runUnionHandler :: forall r rs rs' a. (r `In` rs', rs `AllIn` rs', rs' `AllIn` (r ': rs)) =>
  UnionHandler r rs a -> Union rs' -> a
runUnionHandler h u = case tryRunUnionHandler h u of
  Just a -> a
  Nothing -> error "This should never happen!"

handle :: forall r rs a. Either (Union (r ': rs)) a -> (r -> a) -> Either (Union rs) a
handle (Right a) _ = Right a
handle (Left u) f = case project' u of
  Right r -> Right $ f r
  Left u' -> Left u'

doHandling :: Either (Union '[]) a -> a
doHandling (Right a) = a
doHandling (Left _) = error "Please only use doHandling as part of a list of error handling functions"
