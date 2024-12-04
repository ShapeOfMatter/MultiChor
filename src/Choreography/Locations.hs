-- | This module defines locations and functions/relations pertaining to type-level lists of locations.
module Choreography.Locations where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

-- * Membership and Subset proofs

-- These are frequently used both for proofs *per se* and to indentify individuals in lists of locations.
--
-- For example: `players :: Subset players census` is a proof that the type-level list `players`  is a subset of `census`,
-- and it can also be used as a **term-level** identifier for the **type-level** `players`, similar to how a `proxy` might be used.

-- | A term-level proof that a `LocTy` is a member of a `[LocTy]`.
--   Pattern matching on such these values is like pattern matching on a successor-based `Nat`;
--   in this sense a `Member x xs` is an index into `xs` at which `x` can be found.
data Member (x :: k) (xs :: [k]) where
  First :: forall xs x xs'. (xs ~ (x ': xs')) => Member x (x ': xs')
  Later :: Member x xs -> Member x (y ': xs)

-- | A term level proof that one type-level list represents a subset of another,
--   embodied by a total function from proof-of-membership in the sublist to proof-of-membership in the superlist.
newtype Subset xs ys = Subset
  { -- | Convert a proof of membership in the sublist to a proof of membership in the superlist.
    -- Frequently used to show that a location is part of a larger set of locations.
    inSuper :: forall x. Member x xs -> Member x ys
  }

-- | The subset relation is reflexive.
refl :: Subset xs xs
refl = Subset id

-- | The sublist relation is transitive.
transitive :: Subset xs ys -> Subset ys zs -> Subset xs zs
transitive sxy syz = Subset $ inSuper syz . inSuper sxy

-- | The `[]` case of subset proofs.
-- Typlically used to build subset proofs using membership proofs using `@@`.
nobody :: Subset '[] ys
nobody = Subset \case {}

-- | Any lists is a subset of the list made by consing itself with any other item.
consSet :: forall xs x xs'. (xs ~ (x ': xs')) => Subset xs' (x ': xs')
consSet = Subset Later

-- | Cons an element to the superset in a `Subset` value.
consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet

-- | Cons an element to the subset in a `Subset` value; requires proof that the new head element is already a member of the superset.
--   Used like `:` for subset proofs.
--   Suppose you have (alice :: Member "Alice" census) and we want a subset proof with alice in the census then we can do:
--   >>> proof :: Subset '["Alice"]  census  = alice @@ nobody
(@@) :: Member x ys -> Subset xs ys -> Subset (x ': xs) ys

infixr 5 @@

mxy @@ sxy = Subset \case
  First -> mxy
  Later mxxs -> inSuper sxy mxxs

-- * Accessing parties' names

-- | Convert a proof-level location to a term-level location.
toLocTm ::
  forall (l :: LocTy) (ps :: [LocTy]).
  (KnownSymbol l) =>
  Member l ps ->
  LocTm
toLocTm _ = symbolVal (Proxy @l)

-- | Get the term-level list of names-as-strings for a proof-level list of parties.
toLocs :: forall (ls :: [LocTy]) (ps :: [LocTy]). (KnownSymbols ls) => Subset ls ps -> [LocTm]
toLocs _ = case tySpine @ls of -- this could be golfed by Quire, if that were defined here.
  TyNil -> []
  TyCons -> toLocTm (First @ls) : toLocs (consSet @ls)

-- * Handling type-level lists literals

-- `KnownSymobls` constraints will often need to be declared in user code,
-- but using `tySpine` should only be neccessary
-- when the behavior of the choreography depends on the structure of the type-level lists.
-- Most of the time the functions in `Choreography.Polymorphism` should do this for you.

-- | Term-level markers of the spine/structure of a type-level list.
--   Pattern matching on them recovers both the spine of the list and, if applicable,
--   `KnownSymbol[s]` instances for the head and tail.
data TySpine ps where
  -- | Denotes that the list has a head and tail, and exposes `KnownSymbol` and `KnownSymbols` constraints respectively.
  TyCons :: (KnownSymbol h, KnownSymbols ts) => TySpine (h ': ts)
  -- | Denotes that the list is empty.
  TyNil :: TySpine '[]

-- | The type-level-list version of GHC.TypeList.KnownSymbol.
--   Denotes that both the spine of the list and each of its elements is known at compile-time.
--   This knowlege is typically recovered by recursively pattern-matching on `tySpine @ls`.
class KnownSymbols ls where
  -- | Pattern matching on `tySpine @ls` will normally have two cases, for when `ls` is empty or not.
  --   Contextual knowlege may let one or the other case be skipped.
  --   Within those casese, the knowlege afforded by `tySpine`'s constructors can be used.
  tySpine :: TySpine ls

instance KnownSymbols '[] where
  tySpine = TyNil

instance (KnownSymbols ls, KnownSymbol l) => KnownSymbols (l ': ls) where
  tySpine = TyCons
