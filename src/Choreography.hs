module Choreography (
  module Choreography.Core,
  module Choreography.Choreography,
  module Choreography.Choreography.Batteries,
  module Choreography.Locations,
  module Choreography.Locations.Batteries,
  module Choreography.Network,
  module Choreography.Polymorphism,

  -- * Running choreographies
  runChoreography
  ) where

import Control.Monad.IO.Class (MonadIO)

import Choreography.Core
import Choreography.Choreography
import Choreography.Choreography.Batteries
import Choreography.Locations
import Choreography.Locations.Batteries
import Choreography.Network
import Choreography.Polymorphism

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m, KnownSymbols ps) => config -> Choreo ps m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)

