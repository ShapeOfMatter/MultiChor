{-# LANGUAGE TypeFamilies #-}
{-|
This is the membrane between internal and external code; only here and /src/Network.hs should import from internal,
and everything they export should be safe for users to use.
-}
module Choreography.Core (
    Choreo
  , comm
  , congruently
  , consSet
  , consSub
  , consSuper
  , enclave
  , epp
  , ExplicitMember
  , explicitMember
  , ExplicitSubset
  , explicitSubset
  , Faceted()
  , fanIn
  , fanOut
  , flatten
  , fracture
  , inSuper
  , type IsMember
  , type IsSubset
  , KnownSymbols()
  , localize
  , LocTm
  , LocTy
  , Located()
  , Member
  , naked
  , parallel
  , runChoreo
  , Subset
  , toLocTm
  , toLocs
  , Unwrap
  , Unwraps
  , Wrapped()
) where

import Choreography.Internal.Choreo
import Choreography.Internal.Location


