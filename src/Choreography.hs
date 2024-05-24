{-# LANGUAGE ExplicitNamespaces #-}

module Choreography (
  module Choreography.Choreo,
  module Choreography.Core,
  module Choreography.Location,

  -- * Running choreographies
  runChoreography
  ) where

import Choreography.Core
import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Control.Monad.IO.Class

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo ps m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)

