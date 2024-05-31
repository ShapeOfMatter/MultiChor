{-# LANGUAGE TemplateHaskell #-}
module ExampleChor where

import Choreography
import Choreography.Network.Http
import CLI
import GDP

$(mkLoc "alice")
$(mkLoc "bob")
$(mkLoc "carroll")

-- | Figure 2: An example choreography, written with MultiChor, in which an Int, originating at a party bob, is passed back
-- and forth. Ultimately, a version of that value owned only by carroll is returned. Part of the choreography takes place in
-- an enclave involving some collection of parties clique.
exampleChor :: Choreo census m (Located carroll Int)  -- The inner monad m is not used here.
exampleChor = do
  let clique = consSub refl bob -- TODO a subset of census but also includes bob
  theirFoo <- clique `enclave` foo  -- Lift a Choreo involving only the parties in clique
                                    --  into a Choreo of census.
  (bob, theirFoo) ~> carroll @@ nobody -- Bob, an owner of theirFoo, sends it to Carroll
                                          --  (and nobody else).
  where foo = do
                aliceFoo <- (bob, bobFoo) ~> alice @@ nobody  -- Bob sends his value bobFoo
                                                                --  to Alice and nobody else.
                broadcast (alice, aliceFoo)  -- Alice, an owner of aliceFoo, sends it to the census
                                                --  of foo (_not_ the census of exampleChor).
