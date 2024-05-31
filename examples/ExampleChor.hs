{-# LANGUAGE TemplateHaskell #-}
module ExampleChor where

import Control.Monad.Identity (runIdentity)

import Choreography

$(mkLoc "alice")
$(mkLoc "bob")
$(mkLoc "carroll")

type Census = '["alice", "bob", "jeff", "carroll", "tom"]
type Clique = '["alice", "bob", "jeff"]

clique :: Subset Clique Census
clique = explicitSubset

bobFoo :: Located '["bob"] Int
bobFoo = runIdentity . runChoreo $ bobb `_locally` pure 5
  where bobb :: Member "bob" Clique
        bobb = bob

log :: Located '["carroll"] Int ->  Choreo Census IO ()
log i = carroll `locally_` \un -> print $ un singleton i

-- | Figure 2: An example choreography, written with MultiChor, in which an Int, originating at a party bob, is passed back
-- and forth. Ultimately, a version of that value owned only by carroll is returned. Part of the choreography takes place in
-- an enclave involving some collection of parties clique.
exampleChor :: Choreo Census m (Located '["carroll"] Int)  -- The inner monad m is not used here.
exampleChor = do
    theirFoo <- clique `enclave` foo  -- Lift a Choreo of some clique into a Choreo of Census.
    (bob, theirFoo) ~> carroll @@ nobody  -- Bob sends `theirFoo` to Carroll (and nobody else).
  where foo = do
          aliceFoo <- (bob, bobFoo) ~> alice @@ nobody  -- Bob sends his value `bobFoo` to Alice.
          broadcast (alice, aliceFoo)  -- Alice sends it to the census of `foo`,
                                       -- which is `clique`, _not_ `Census`.
