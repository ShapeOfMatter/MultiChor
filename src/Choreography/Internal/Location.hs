{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Internal.Location where

import Data.Foldable (toList)
import Data.Functor.Const (Const(Const), getConst)
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

-- | Unwraps values known to the specified party or parties.
type Unwrap (q :: LocTy) = forall ls a. Member q ls -> Located ls a -> a  -- This is wrong! should use membership to avoid empty sets.
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a  -- This is wrong! should use membership to avoid empty sets.

-- | Unwrap a `Located` value.
--Unwrapping a empty located value will throw an exception!
unwrap :: Unwrap q
unwrap _ (Wrap a) = a
unwrap _ Empty    = error "Located: This should never happen for a well-typed choreography."


data Member (x :: k) (xs :: [k]) where
  First :: Member x (x ': xs)
  Later :: Member x xs -> Member x (y ': xs)
newtype Subset xs ys = Subset { inSuper  :: forall x. Member x xs -> Member x ys }
refl :: Subset xs xs
refl = Subset id
transitive :: Subset xs ys -> Subset ys zs -> Subset xs zs
transitive sxy syz = Subset $ inSuper syz . inSuper sxy

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
  Later mxxs -> inSuper sxy mxxs


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

type PIndex ls f = forall l. (KnownSymbol l) => Member l ls -> f l
newtype PIndexed ls f = PIndexed {pindex :: PIndex ls f}

-- | A collection of values assigned to each of a list of parties.
newtype Quire parties a = Quire {asPIntexed :: PIndexed parties (Const a)}
getLeaf :: (KnownSymbol p) => Quire parties a -> Member p parties -> a
getLeaf (Quire (PIndexed q)) p = getConst $ q p
stackLeaves :: forall ps a. (forall p. (KnownSymbol p) => Member p ps -> a) -> Quire ps a
stackLeaves f = Quire $ PIndexed $ Const . f
qHead :: (KnownSymbol p) => Quire (p ': ps) a -> a
qHead (Quire (PIndexed f)) = getConst $ f First
qTail :: Quire (p ': ps) a -> Quire ps a
qTail (Quire (PIndexed f)) = Quire $ PIndexed $ f . Later
qCons :: forall p ps a. a -> Quire ps a -> Quire (p ': ps) a
qCons a (Quire (PIndexed f)) = Quire $ PIndexed $ \case
  First -> Const a
  Later mps -> f mps
qNil :: Quire '[] a
qNil = Quire $ PIndexed \case {}

instance forall parties. (KnownSymbols parties) => Functor (Quire parties) where
  fmap f q = case tyUnCons @parties of
               TyCons -> f (qHead q) `qCons` fmap f (qTail q)
               TyNil -> qNil
instance forall parties. (KnownSymbols parties) => Applicative (Quire parties) where
  pure a = Quire $ PIndexed $ const $ Const a
  qf <*> qa = case tyUnCons @parties of
                TyCons -> qHead qf (qHead qa) `qCons` (qTail qf <*> qTail qa)
                TyNil -> qNil
instance forall parties. (KnownSymbols parties) => Foldable (Quire parties) where
  foldMap f q = case tyUnCons @parties of
                  TyCons -> f (qHead q) <> foldMap f (qTail q)
                  TyNil -> mempty
instance forall parties. (KnownSymbols parties) => Traversable (Quire parties) where
  sequenceA q = case tyUnCons @parties of
                  TyCons -> qCons <$> qHead q <*> sequenceA (qTail q)
                  TyNil -> pure qNil
-- Many more instances are possible...

qModify :: forall p ps a. (KnownSymbol p, KnownSymbols ps) =>  Member p ps -> (a -> a) -> Quire ps a -> Quire ps a
qModify First f q = f (qHead q) `qCons` qTail q
qModify (Later m) f q = case tyUnCons @ps of TyCons -> qHead q `qCons` qModify m f (qTail q)


-- | Get the term-level list of names-as-strings for a proof-level list of parties.
toLocs :: forall (ls :: [LocTy]) (ps :: [LocTy]). KnownSymbols ls => Subset ls ps -> [LocTm]
toLocs _ = toList $ stackLeaves @ls toLocTm

-- | Un-nest located values.
flatten :: Subset ls ms -> Subset ls ns -> Located ms (Located ns a) -> Located ls a
infix 3 `flatten`
flatten _ _ Empty = Empty
flatten _ _ (Wrap Empty) = Empty
flatten _ _ (Wrap (Wrap a)) = Wrap a

othersForget :: Subset ls owners -> Located owners a -> Located ls a
othersForget _ Empty = Empty
othersForget _ (Wrap a) = Wrap a
