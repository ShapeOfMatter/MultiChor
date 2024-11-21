-- | This module defines locations and functions/relations pertaining to type-level lists of locations.
--   Additionally introduces `PIndexed` and `Quire`.
module Choreography.Locations.Batteries where

import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH

import Choreography.Locations

-- | Quickly build membership proofs, when the membership can be directly observed by GHC.
class ExplicitMember (x :: k) (xs :: [k]) where
  explicitMember :: Member x xs
instance {-# OVERLAPPABLE #-} (ExplicitMember x xs) =>  ExplicitMember x (y ': xs) where
  explicitMember = inSuper consSet explicitMember
instance {-# OVERLAPS #-} ExplicitMember x (x ': xs) where
  explicitMember = First

-- | Quickly build subset proofs, when the subset relation can be directly observed by GHC.
class ExplicitSubset xs ys where
  explicitSubset :: Subset xs ys
instance {-# OVERLAPPABLE #-} (ExplicitSubset xs ys, ExplicitMember x ys) => ExplicitSubset (x ': xs) ys where
  explicitSubset = explicitMember @@ explicitSubset
instance {-# OVERLAPS #-} ExplicitSubset '[] ys where
  explicitSubset = nobody


-- | Alias `refl`. When used as an identifier, this is more descriptive.
allOf :: forall ps. Subset ps ps
allOf = refl

-- | Any element `p` is a member of the list `'[p]`.
singleton :: forall p. Member p (p ': '[])
singleton = First

-- * Easy indexing with `Member` objects.

listedFirst :: forall p1 ps. Member p1 (p1 ': ps)  -- Can we replace all of these with something using off-the-shelf type-level Nats?
listedFirst = First                                -- Additionally, note that type-applicaiton is different than with `First`.

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


-- | Use any membership proof to to safely call code that only works on a non-empy list.
quorum1 :: forall ps p a.
           (KnownSymbols ps)
        => Member p ps
        -> (forall q qs. (KnownSymbol q, KnownSymbols qs, ps ~ q ': qs) => a)
        -> a
quorum1 p a = case (p, tySpine @ps) of (First, TyCons) -> a
                                       (Later _, TyCons) -> a


-- | Declare a proof-value with the given string as the variable name, proving that that string is a member of any list in which it explicitly apprears.
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

