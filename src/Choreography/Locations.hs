-- | This module defines locations and functions/relations pertaining to type-level lists of locations.
--   Additionally introduces `PIndexed` and `Quire`.
module Choreography.Locations where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol


-- | A term-level proof that a `LocTy` is a member of a `[LocTy]`.
--   Pattern matching on such a term recovers knowlege of the list's structure.
data Member (x :: k) (xs :: [k]) where
  First :: forall xs x xs'. (xs ~ (x ': xs')) => Member x (x ': xs')
  Later :: Member x xs -> Member x (y ': xs)


-- | A term level proof that one type-level list represents a subset of another is
--   embodied by a total function from proof-of-membership in the sublist to proof-of-membership in the superlist.
newtype Subset xs ys = Subset { inSuper :: forall x. Member x xs -> Member x ys }

-- | The subset relation is reflexive.
refl :: Subset xs xs
refl = Subset id

-- | The sublist relation is transitive.
transitive :: Subset xs ys -> Subset ys zs -> Subset xs zs
transitive sxy syz = Subset $ inSuper syz . inSuper sxy

-- | The `[]` case of subset proofs.
nobody :: Subset '[] ys
nobody = Subset \case {}  -- might be a nicer way to write that...

consSet :: forall xs x xs'. (xs ~ (x ': xs')) => Subset xs' (x ': xs')
consSet = Subset Later

consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet

consSub :: Subset xs ys -> Member x ys -> Subset (x ': xs) ys
consSub sxy mxy = Subset \case
  First -> mxy
  Later mxxs -> inSuper sxy mxxs


-- | Term-level markers of the spine/structure of a type-level list.
--   Pattern matching on them recovers both the spine of the list and, if applicable,
--   `KnownSymbol[s]` instances for the head and tail.
data TyUnCons ps where
    TyCons :: (KnownSymbol h, KnownSymbols ts) => TyUnCons (h ': ts)
    TyNil :: TyUnCons '[]

-- | The type-level-list version of GHC.TypeList.KnownSymbol.
--   Denotes that both the spine of the list and each of its elements is known at compile-time.
--   This knowlege is typically recovered by recursively pattern-matching on `tyUnCons @ls`.
class KnownSymbols ls where
  tyUnCons :: TyUnCons ls

instance KnownSymbols '[] where
  tyUnCons = TyNil

instance (KnownSymbols ls, KnownSymbol l) => KnownSymbols (l ': ls) where
  tyUnCons = TyCons


-- | Convert a proof-level location to a term-level location.
toLocTm :: forall (l :: LocTy) (ps :: [LocTy]).
           KnownSymbol l
        => Member l ps
        -> LocTm
toLocTm _ = symbolVal (Proxy @l)

-- | Get the term-level list of names-as-strings for a proof-level list of parties.
toLocs :: forall (ls :: [LocTy]) (ps :: [LocTy]). KnownSymbols ls => Subset ls ps -> [LocTm]
toLocs _ = case tyUnCons @ls of  -- this could be golfed by Quire, if that were defined here.
  TyNil -> []
  TyCons -> toLocTm (First @ls) : toLocs (consSet @ls)

