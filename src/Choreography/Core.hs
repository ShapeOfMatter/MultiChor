{-# LANGUAGE TypeFamilies #-}
{-|
This is the membrane between internal and external code; only here and /src/Network.hs should import from internal,
and everything they export should be safe for users to use.
-}
module Choreography.Core (
    alone
  , broadcast'
  , Choreo
  , consSet
  , consSub
  , consSuper
  , enclave
  , epp
  , flatten
  , getLeaf
  , inSuper
  , KnownSymbols(..)
  , LocTm
  , LocTy
  , Located()
  , Member(..)
  , nobody
  , othersForget
  , PIndex
  , PIndexed(..)
  , purely
  , qCons
  , qHead
  , qModify
  , qNil
  , qTail
  , Quire(..)
  , refl
  , runChoreo
  , sequenceP
  , Subset
  , stackLeaves
  , toLocTm
  , toLocs
  , transitive
  , TyUnCons(..)
  , Unwrap
  , Unwraps
) where

import Choreography.Internal.Choreo
import Choreography.Internal.Location


