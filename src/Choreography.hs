-- | This is just a wrapper module to allow more concise imports.
--     For documentation, you should probably start in "Choreography.Core".
module Choreography
  ( module Choreography.Core,
    module Choreography.Choreography,
    module Choreography.Choreography.Batteries,
    module Choreography.Network,
    module Choreography.Polymorphism,

    -- * Running choreographies
    runChoreography,
  )
where

import Choreography.Choreography
import Choreography.Choreography.Batteries
import Choreography.Core
import Choreography.Network
import Choreography.Polymorphism
import Control.Monad.IO.Class (MonadIO)
import Data.Known.Knowable

-- | Run a choreography with a message transport backend...
runChoreography :: (Backend config, MonadIO m, Known [Party] ps) => config ps -> Choreo ps m a -> PartyName -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
