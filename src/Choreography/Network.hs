-- | This module exposes the Network monad.
-- Only here and Core should import from internal, and everything they expose should be safe for end-users.
-- Network is a little more permissive about exporting the signature data type because you need to reference it to write new handlers.
module Choreography.Network (
    -- broadcast
    Backend(runNetwork)
  , Network
  , NetworkSig(..)
  , recv
  , run
  , send

) where

import Choreography.Internal.Network as N

