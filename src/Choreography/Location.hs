{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Location (
    allOf
  , ExplicitMember
  , explicitMember
  , ExplicitSubset
  , explicitSubset
  , Facet(..)
  , Faceted
  , listedFirst, listedSecond, listedThird, listedForth, listedFifth, listedSixth
  , localize
  , mkLoc
  , quorum1
  , singleton
  , viewFacet
  , (@@)
) where

import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH

import Choreography.Core


allOf :: forall ps. Subset ps ps
allOf = refl

-- | Use like `:` for subset proofs.
(@@) :: Member x ys -> Subset xs ys -> Subset (x ': xs) ys
infixr 5 @@
a @@ bs = bs `consSub` a  -- SHould be able to use flip?


singleton :: forall p. Member p (p ': '[])
singleton = listedFirst  -- IKD why I can't just use id.

listedFirst :: forall p1 ps. Member p1 (p1 ': ps)  -- Can we replace all of these with something using off-the-shelf type-level Nats?
listedFirst = First

listedSecond :: forall p2 p1 ps. Member p2 (p1 ': p2 ': ps)
listedSecond = inSuper (consSuper refl) listedFirst

listedThird :: forall p3 p2 p1 ps. Member p3 (p1 ': p2 ': p3 ': ps)
listedThird = inSuper (consSuper refl) listedSecond

listedForth :: forall p4 p3 p2 p1 ps. Member p4 (p1 ': p2 ': p3 ': p4 ': ps)
listedForth = inSuper (consSuper refl) listedThird

listedFifth :: forall p5 p4 p3 p2 p1 ps. Member p5 (p1 ': p2 ': p3 ': p4 ': p5 ': ps)
listedFifth = inSuper (consSuper refl) listedForth

listedSixth :: forall p6 p5 p4 p3 p2 p1 ps. Member p6 (p1 ': p2 ': p3 ': p4 ': p5 ': p6 ': ps)
listedSixth = inSuper (consSuper refl) listedFifth


class ExplicitMember (x :: k) (xs :: [k]) where
  explicitMember :: Member x xs
instance {-# OVERLAPPABLE #-} (ExplicitMember x xs) =>  ExplicitMember x (y ': xs) where
  explicitMember = inSuper consSet explicitMember
instance {-# OVERLAPS #-} ExplicitMember x (x ': xs) where
  explicitMember = First

class ExplicitSubset xs ys where
  explicitSubset :: Subset xs ys
instance {-# OVERLAPPABLE #-} (ExplicitSubset xs ys, ExplicitMember x ys) => ExplicitSubset (x ': xs) ys where
  explicitSubset = consSub explicitSubset explicitMember
instance {-# OVERLAPS #-} ExplicitSubset '[] ys where
  explicitSubset = nobody



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

-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
type Faceted parties common a = PIndexed parties (Facet a common)
newtype Facet a common p = Facet {getFacet :: Located (p ': common) a}

-- | Get a `Located` value of a `Faceted` at a given location.
localize :: (KnownSymbol l) => Member l ls -> Faceted ls common a -> Located (l ': common) a
localize l (PIndexed f) = getFacet $ f l

viewFacet :: (KnownSymbol l) => Unwrap l -> Member l ls -> Faceted ls common a -> a
viewFacet un l = un First . localize l

{- -- It seems like we should still want Wrapped for something....
class Wrapped w where
  -- | Unwrap a `Located` or a `Faceted`. Can error if misused.
  unwrap' :: Member l ls -> w ls a -> a

instance Wrapped Located where
  unwrap' = unwrap . consSub nobody

instance Wrapped Faceted where
  unwrap' = mine
-}

{-
unsafeFacet :: [Maybe a] -> Member l ls -> Facet a common l -- providing this as a helper function is pretty sketchy, if we don't need it delete it.
unsafeFacet (Just a : _) First = Facet $ wrap a
unsafeFacet (Nothing : _) First = Empty
unsafeFacet (_ : as) (Later l) = unsafeFacet as l
unsafeFacet [] _ = error "The provided list isn't long enough to use as a Faceted over the intended parties."
-}

{-  -- These are probably useless...
-- | Unwrap a `Faceted` value.
mine :: Member l ls -> Faceted ls a -> a
mine l (FacetF f) = unwrap refl $ f l

-- | Use a `Located` as a `Faceted`.
fracture :: forall ls a. (KnownSymbols ls) => Located ls a -> Faceted ls a
fracture a = FacetF \_ -> case a of
                            Empty -> Empty
                            Wrap a' -> Wrap a'
-}

quorum1 :: forall ps p a.
           (KnownSymbols ps)
        => Member p ps -> (forall q qs. (KnownSymbol q, KnownSymbols qs, ps ~ q ': qs) => a) -> a
quorum1 p a = case (p, tyUnCons @ps) of (First, TyCons) -> a
                                        (Later _, TyCons) -> a

