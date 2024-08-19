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
  , forLocs
  , inSuper
  , KnownSymbols()
  , listedFirst
  , LocTm
  , LocTy
  , Located()
  , Member
  , memberships
  , nobody
  , purely
  , refl
  , runChoreo
  , Subset
  , toLocTm
  , toLocs
  , transitive
  , Unwrap
  , Unwraps
) where

import Choreography.Internal.Choreo
import Choreography.Internal.Location


