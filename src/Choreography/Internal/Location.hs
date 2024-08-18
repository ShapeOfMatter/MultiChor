{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Internal.Location where

--import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
--import Logic.Proof (Proof, axiom)
--import Logic.Classes (Reflexive, refl, Transitive, transitive)

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

-- | Unwraps values known to the specified party or parties.
type Unwrap (q :: LocTy) = forall ls a. Member q ls -> Located ls a -> a  -- This is wrong! should use membership to avoid empty sets.
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a  -- This is wrong! should use membership to avoid empty sets.

-- | Unwrap a `Located` value.
--Unwrapping a empty located value will throw an exception!
unwrap :: Unwrap q
unwrap _ (Wrap a) = a
unwrap _ Empty    = error "Located: This should never happen for a well-typed choreography."
--unwraps :: Unwraps ls
--unwrap qs la = 
--unwraps _ (Wrap a) = a
--unwraps _ Empty    = error "Located: This should never happen for a well-typed choreography."


-- GDP has its own list logic, but IDK how to use it...
data Member (x :: k) (xs :: [k]) where
  First :: Member x (x ': xs)
  Later :: Member x xs -> Member x (y ': xs)
--data IsMember (x :: k) (xs :: [k]) where {}
--type Member x xs = Proof (IsMember x xs)
newtype Subset xs ys = Subset { memberships :: forall x. Member x xs -> Member x ys }
--data IsSubset (xs :: [k]) (ys :: [k]) where {}
--type Subset xs ys = Proof (IsSubset xs ys)
refl :: Subset xs xs
refl = Subset id
transitive :: Subset xs ys -> Subset ys zs -> Subset xs zs
transitive sxy syz = Subset $ memberships syz . memberships sxy
--instance Reflexive IsSubset where {}
--instance Transitive IsSubset where {}

{-class ExplicitMember (x :: k) (xs :: [k]) where
  explicitMember :: Member x xs
instance {-# OVERLAPPABLE #-} (ExplicitMember x xs) =>  ExplicitMember x (y ': xs) where
  explicitMember = inSuper consSet explicitMember
instance {-# OVERLAPS #-} ExplicitMember x (x ': xs) where
  explicitMember = axiom-}

listedFirst :: forall p1 ps. Member p1 (p1 ': ps)
listedFirst = First

-- | The `[]` case of subset proofs.
nobody :: Subset '[] ys
nobody = Subset \case {}  -- might be a nicer way to write that...

consSet :: Subset xs (x ': xs)
consSet = Subset Later

consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet

consSub :: Subset xs ys -> Member x ys -> Subset (x ': xs) ys
consSub sxy mxy = Subset \case
  First -> mxy
  Later mxxs -> memberships sxy mxxs
--consSub = const $ const axiom

inSuper :: Subset xs ys -> Member x xs -> Member x ys
inSuper (Subset sxy) = sxy -- Why can't this just be id?

{-class ExplicitSubset xs ys where
  explicitSubset :: Subset xs ys

instance {-# OVERLAPPABLE #-} (ExplicitSubset xs ys, ExplicitMember x ys) => ExplicitSubset (x ': xs) ys where
  explicitSubset = consSub explicitSubset explicitMember
instance {-# OVERLAPS #-} ExplicitSubset '[] ys where
  explicitSubset = axiom-}


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
  --tyUnCons = TyCons (explicitMember @Symbol @l @(l ': ls)) $ consSuper refl

-- | Map a function, which takes proof of membership as its argument, over a proof-specified list of locations.
mapLocs :: forall (ls :: [LocTy]) b (ps :: [LocTy]). KnownSymbols ls => (forall l. (KnownSymbol l) => Member l ls -> b) -> Subset ls ps -> [b]
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

