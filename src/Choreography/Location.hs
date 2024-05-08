{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Location where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Language.Haskell.TH
import Logic.Proof (Proof, axiom)
import Logic.Propositional (type (&&))
import Logic.Classes (Reflexive, refl, Transitive, transitive)

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
unwrap :: Subset qs ls -> Located ls a -> a
unwrap _ (Wrap a) = a
unwrap _ Empty    = error "Located: This should never happen for a well-typed choreography."

-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
newtype Faceted (ls :: [LocTy]) a = Faceted [(LocTm, a)]

-- | Unwrap a `Faceted` value.
--Unwrapping an empty faceted value will throw an exception!
mine :: (KnownSymbol l) => Member l ls -> Faceted ls a -> a
mine l (Faceted facets) = fromMaybe (error "Faceted: This should never happen for a well-typed choreography.")
                                    $ toLocTm l `lookup` facets


-- GDP has its own list logic, but IDK how to use it...
data IsMember (x :: k) (xs :: [k]) where {}
type Member x xs = Proof (IsMember x xs)
data IsSubset (xs :: [k]) (ys :: [k]) where {}
type Subset xs ys = Proof (IsSubset xs ys)
instance Reflexive IsSubset where {}
instance Transitive IsSubset where {}

class ExplicitMember (x :: k) (xs :: [k]) where
  explicitMember :: Member x xs
instance {-# OVERLAPPABLE #-} (ExplicitMember x xs) =>  ExplicitMember x (y ': xs) where
  explicitMember = inSuper consSet explicitMember
instance {-# OVERLAPS #-} ExplicitMember x (x ': xs) where
  explicitMember = axiom

consSet :: Subset xs (x ': xs)
consSet = consSuper refl  -- these are circular, is that bad?
consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet
consSub :: Subset xs ys -> Member x ys -> Subset (x ': xs) ys
consSub = const $ const axiom
inSuper :: Subset xs ys -> Member x xs -> Member x ys
inSuper _ _ = axiom

class ExplicitSubset xs ys where
  explicitSubset :: Subset xs ys

instance {-# OVERLAPPABLE #-} (ExplicitSubset xs ys, ExplicitMember x ys) => ExplicitSubset (x ': xs) ys where
  explicitSubset = consSub explicitSubset explicitMember
instance {-# OVERLAPS #-} ExplicitSubset '[] ys where
  explicitSubset = axiom

-- | The `[]` case of subset proofs.
nobody :: Subset '[] ys
nobody = explicitSubset

-- | Use like `:` for subset proofs.
(@@) :: Member x ys -> Subset xs ys -> Subset (x ': xs) ys
infixr 5 @@
(@@) = flip consSub

-- |Declare a proof-value with the given string as the variable name, proving that that string is a member of any list in which it explicitly apprears.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let tvar = mkName "xs"
  let m = mkName "Member"
  let eM = mkName "ExplicitMember"
  let em = mkName "explicitMember"
  pure [ SigD locName (ForallT [PlainTV tvar SpecifiedSpec]
                               [AppT (AppT (ConT eM) (LitT (StrTyLit loc))) (VarT tvar)]
                               (AppT (AppT (ConT m) (LitT (StrTyLit loc))) (VarT tvar)))
       , ValD (VarP locName) (NormalB (VarE em)) []
       ]

singleton :: forall p. (forall ps. (ExplicitMember p ps) => Member p ps) -> Member p (p ': '[])
singleton proof = proof  -- IKD why I can't just use id.

-- | Convert a proof-level location to a term-level location.
toLocTm :: forall (l :: LocTy) (ps :: [LocTy]). KnownSymbol l => Member l ps -> LocTm
toLocTm _ = symbolVal (Proxy @l)

data TyUnCons ps = forall h ts. (KnownSymbol h, KnownSymbols ts) => TyCons (Member h ps) (Subset ts ps)
                 | TyNil (Subset ps '[])

class KnownSymbols ls where
  tyUnCons :: TyUnCons ls

instance KnownSymbols '[] where
  tyUnCons = TyNil explicitSubset

instance (KnownSymbols ls, KnownSymbol l) => KnownSymbols (l ': ls) where
  tyUnCons = TyCons (explicitMember @Symbol @l @(l ': ls)) $ consSuper refl

-- | Map a function, which takes proof of membership as its argument, over a proof-specified list of locations.
mapLocs :: forall (ls :: [LocTy]) b (ps :: [LocTy]). KnownSymbols ls => (forall l. (KnownSymbol l) => Member l ls -> b) -> Subset ls ps -> [b]
mapLocs f ls = case tyUnCons @ls of
                 TyCons h ts -> f h :  (f . inSuper ts) `mapLocs` transitive ts ls
                 TyNil _ -> []

-- | Get the term-level list of names-as-strings fror a proof-level list of parties.
toLocs :: forall (ls :: [LocTy]) (ps :: [LocTy]). KnownSymbols ls => Subset ls ps -> [LocTm]
toLocs ls = (\(_ :: KnownSymbol l => Member l ls) -> symbolVal $ Proxy @l) `mapLocs` ls

-- | Un-nest located values.
flatten :: Proof (IsSubset ls ms && IsSubset ls ns) -> Located ms (Located ns a) -> Located ls a
infix 3 `flatten`
flatten _ Empty = Empty
flatten _ (Wrap Empty) = Empty
flatten _ (Wrap (Wrap a)) = Wrap a

-- | Get the singlely-`Located` value of a `Faceted` at a given location.
localize :: (KnownSymbol l) => Member l ls -> Faceted ls a -> Located '[l] a
localize l (Faceted facets) = maybe Empty Wrap $ toLocTm l `lookup` facets

-- | Use a `Located` as a `Faceted`.
fracture :: forall ls a. (KnownSymbols ls) => Located ls a -> Faceted ls a
fracture Empty = Faceted []
fracture (Wrap a) = Faceted $ (, a) <$> toLocs (refl :: Subset ls ls)

class Wrapped w where
  -- | Unwrap a `Located` or a `Faceted`. Can error if misused.
  unwrap' :: (KnownSymbol l) => Member l ls -> w ls a -> a

instance Wrapped Located where
  unwrap' = unwrap . (@@ nobody)

instance Wrapped Faceted where
  unwrap' = mine
