{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Internal.Location where

import Data.Proxy (Proxy(..))
import GHC.TypeLits

-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

-- | A single value known to many parties.
data Located (ls :: [LocTy]) a
  = Wrap a
  | Empty

-- | Wrap a value as a located value.
--Why would one use this instead of the constructor?
wrap :: a -> Located l a
wrap = Wrap

-- | Unwrap a `Located` value.
--Unwrapping a empty located value will throw an exception!
unwrap :: Subset qs ls -> Located ls a -> a  -- This is wrong! should use membership to avoid empty sets.
unwrap _ (Wrap a) = a
unwrap _ Empty    = error "Located: This should never happen for a well-typed choreography."

-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
newtype Faceted (ls :: [LocTy]) a = FacetF (forall l. Member l ls -> Located '[l] a)

unsafeFacet :: [Maybe a] -> Member l ls -> Located '[l] a
unsafeFacet (Just a : _) First = Wrap a
unsafeFacet (Nothing : _) First = Empty
unsafeFacet (_ : as) (Later l) = unsafeFacet as l
unsafeFacet [] _ = error "The provided list isn't long enough to use as a Faceted over the intended parties."


data Member (x :: k) (xs :: [k]) where
  First :: Member x (x ': xs)
  Later :: Member x xs -> Member x (y ': xs)
newtype Subset xs ys = Subset { memberships :: forall x. Member x xs -> Member x ys }
refl :: Subset xs xs
refl = Subset id
transitive :: Subset xs ys -> Subset ys zs -> Subset xs zs
transitive sxy syz = Subset $ memberships syz . memberships sxy

-- | The `[]` case of subset proofs.
nobody :: Subset '[] ys
nobody = Subset \case {}

consSet :: Subset xs (x ': xs)
consSet = Subset Later

consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet

consSub :: Subset xs ys -> Member x ys -> Subset (x ': xs) ys
consSub sxy mxy = Subset \case
  First -> mxy
  Later mxxs -> memberships sxy mxxs

inSuper :: Subset xs ys -> Member x xs -> Member x ys
inSuper (Subset sxy) = sxy


-- | Convert a proof-level location to a term-level location.
toLocTm :: forall (l :: LocTy) (ps :: [LocTy]). KnownSymbol l => Member l ps -> LocTm
toLocTm _ = symbolVal (Proxy @l)

data TyUnCons ps where
    TyCons :: (KnownSymbol h, KnownSymbols ts) => TyUnCons (h ': ts)
    TyNil :: TyUnCons '[]

class KnownSymbols ls where
  tyUnCons :: TyUnCons ls

instance KnownSymbols '[] where
  tyUnCons = TyNil

instance (KnownSymbols ls, KnownSymbol l) => KnownSymbols (l ': ls) where
  tyUnCons = TyCons

-- | Map a function, which takes proof of membership as its argument, over a proof-specified list of locations.
mapLocs :: forall (ls :: [LocTy]) b (ps :: [LocTy]).
           (KnownSymbols ls)
        => (forall l. (KnownSymbol l) => Member l ls -> b)
        -> Subset ls ps
        -> [b]
mapLocs f ls = case tyUnCons @ls of
                 TyCons -> f First : (f . Later) `mapLocs` transitive consSet ls
                 TyNil -> []

-- | Get the term-level list of names-as-strings for a proof-level list of parties.
toLocs :: forall (ls :: [LocTy]) (ps :: [LocTy]). KnownSymbols ls => Subset ls ps -> [LocTm]
toLocs ls = (\(_ :: KnownSymbol l => Member l ls) -> symbolVal $ Proxy @l) `mapLocs` ls

-- | Un-nest located values.
flatten :: Subset ls ms -> Subset ls ns -> Located ms (Located ns a) -> Located ls a
infix 3 `flatten`
flatten _ _ Empty = Empty
flatten _ _ (Wrap Empty) = Empty
flatten _ _ (Wrap (Wrap a)) = Wrap a

-- | Get the singlely-`Located` value of a `Faceted` at a given location.
localize :: Member l ls -> Faceted ls a -> Located '[l] a
localize l (FacetF f) = f l

-- | Unwrap a `Faceted` value.
mine :: Member l ls -> Faceted ls a -> a
mine l (FacetF f) = unwrap refl $ f l

-- | Use a `Located` as a `Faceted`.
fracture :: forall ls a. (KnownSymbols ls) => Located ls a -> Faceted ls a
fracture a = FacetF \_ -> case a of
                            Empty -> Empty
                            Wrap a' -> Wrap a'

class Wrapped w where
  -- | Unwrap a `Located` or a `Faceted`. Can error if misused.
  unwrap' :: Member l ls -> w ls a -> a

instance Wrapped Located where
  unwrap' = unwrap . consSub nobody

instance Wrapped Faceted where
  unwrap' = mine
