{-# LANGUAGE TypeFamilies #-}
{-|
This is the membrane between internal and external code; only here and /src/Network.hs should import from internal,
and everything they export should be safe for users to use.
-}
module Choreography.Core (
    alone
  , Choreo
  , comm
  , consSet
  , consSub
  , consSuper
  , enclave
  , epp
  , flatten
  , inSuper
  , KnownSymbols(tyUnCons)  -- So far I haven't been able to think of a reason this would be unsafe to expose to users...
  , LocTm
  , LocTy
  , Located()
  , Member(..)
  , nobody
  , purely
  , refl
  , runChoreo
  , Subset
  , toLocTm
  , toLocs
  , transitive
  , TyUnCons(TyNil, TyCons)  -- So far I haven't been able to think of a reason this would be unsafe to expose to users...
  , Unwrap
  , Unwraps
  , vacuous
) where

import Choreography.Internal.Choreo
import Choreography.Internal.Location


