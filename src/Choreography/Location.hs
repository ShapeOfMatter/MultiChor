{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and located values.
module Choreography.Location (
    mkLoc
  , nobody
  , singleton
  , (@@)
) where

import Language.Haskell.TH

import Choreography.Core


-- | The `[]` case of subset proofs.
nobody :: Subset '[] ys
nobody = explicitSubset

-- | Use like `:` for subset proofs.
(@@) :: Member x ys -> Subset xs ys -> Subset (x ': xs) ys
infixr 5 @@
(@@) = flip consSub


singleton :: forall p. (forall ps. (ExplicitMember p ps) => Member p ps) -> Member p (p ': '[])
singleton proof = proof  -- IKD why I can't just use id.



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


