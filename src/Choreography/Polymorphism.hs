module Choreography.Polymorphism where

import Control.Monad (void)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Const (Const(Const, getConst))
import GHC.TypeLits

import Choreography.Core
import Choreography.Choreography
import Choreography.Choreography.Batteries ((*~>))
import Choreography.Locations
import Choreography.Locations.Batteries((@@))

-- | A mapping, accessed by `Member` terms, from types (`Symbol`s) to values.
--   The types of the values depend on the indexing type; this relation is expressed by the type-level function `f`.
newtype PIndexed ls f = PIndexed {pindex :: PIndex ls f}
type PIndex ls f = forall l. (KnownSymbol l) => Member l ls -> f l

sequenceP :: forall b (ls :: [LocTy]) m.
           (KnownSymbols ls, Monad m)
        => PIndexed ls (Compose m b)
        -> m (PIndexed ls b)
sequenceP (PIndexed f) = case tyUnCons @ls of
                 TyCons -> do b <- getCompose $ f First
                              PIndexed fTail <- sequenceP (PIndexed $ f . Later)
                              let retVal :: PIndex ls b
                                  retVal First = b
                                  retVal (Later ltr) = fTail ltr
                              return $ PIndexed retVal
                 TyNil -> return $ PIndexed \case {}


-- | A collection of values, all of the same type, assigned to each element of the type-level list.
newtype Quire parties a = Quire {asPIndexed :: PIndexed parties (Const a)}

getLeaf :: (KnownSymbol p) => Quire parties a -> Member p parties -> a
getLeaf (Quire (PIndexed q)) p = getConst $ q p

-- | Package a function as a `Quire`.
stackLeaves :: forall ps a. (forall p. (KnownSymbol p) => Member p ps -> a) -> Quire ps a
stackLeaves f = Quire $ PIndexed $ Const . f

-- | Get the head item from a `Quire`.
qHead :: (KnownSymbol p) => Quire (p ': ps) a -> a
qHead (Quire (PIndexed f)) = getConst $ f First

-- | Get the tail of a `Quire`.
qTail :: Quire (p ': ps) a -> Quire ps a
qTail (Quire (PIndexed f)) = Quire $ PIndexed $ f . Later

-- | Prepend a value to a `Quire`.
--   The corresponding `Symbol` to bind it to must be provided by type-application if it can't be infered.
qCons :: forall p ps a. a -> Quire ps a -> Quire (p ': ps) a
qCons a (Quire (PIndexed f)) = Quire $ PIndexed $ \case
  First -> Const a
  Later mps -> f mps

-- | An empty `Quire`.
qNil :: Quire '[] a
qNil = Quire $ PIndexed \case {}

-- | Apply a function to a single item in a `Quire`.
qModify :: forall p ps a. (KnownSymbol p, KnownSymbols ps) =>  Member p ps -> (a -> a) -> Quire ps a -> Quire ps a
qModify First f q = f (qHead q) `qCons` qTail q
qModify (Later m) f q = case tyUnCons @ps of TyCons -> qHead q `qCons` qModify m f (qTail q)

instance forall parties. (KnownSymbols parties) => Functor (Quire parties) where
  fmap f q = case tyUnCons @parties of
               TyCons -> f (qHead q) `qCons` fmap f (qTail q)
               TyNil -> qNil
instance forall parties. (KnownSymbols parties) => Applicative (Quire parties) where
  pure a = Quire $ PIndexed $ const $ Const a
  qf <*> qa = case tyUnCons @parties of
                TyCons -> qHead qf (qHead qa) `qCons` (qTail qf <*> qTail qa)
                TyNil -> qNil
instance forall parties. (KnownSymbols parties) => Foldable (Quire parties) where
  foldMap f q = case tyUnCons @parties of
                  TyCons -> f (qHead q) <> foldMap f (qTail q)
                  TyNil -> mempty
instance forall parties. (KnownSymbols parties) => Traversable (Quire parties) where
  sequenceA q = case tyUnCons @parties of
                  TyCons -> qCons <$> qHead q <*> sequenceA (qTail q)
                  TyNil -> pure qNil
instance forall parties a. (KnownSymbols parties, Eq a) => Eq (Quire parties a) where
  q1 == q2 = and $ (==) <$> q1 <*> q2
instance forall parties a. (KnownSymbols parties, Show a) => Show (Quire parties a) where
  show q = show $ toLocs (refl @parties) `zip` toList q
-- Many more instances are possible...


-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
type Faceted parties common a = PIndexed parties (Facet a common)
newtype Facet a common p = Facet {getFacet :: Located (p ': common) a}

-- | Get a `Located` value of a `Faceted` at a given location.
localize :: (KnownSymbol l) => Member l ls -> Faceted ls common a -> Located (l ': common) a
localize l (PIndexed f) = getFacet $ f l

viewFacet :: (KnownSymbol l) => Unwrap l -> Member l ls -> Faceted ls common a -> a
viewFacet un l = un First . localize l

{-
unsafeFacet :: [Maybe a] -> Member l ls -> Facet a common l -- providing this as a helper function is pretty sketchy, if we don't need it delete it.
unsafeFacet (Just a : _) First = Facet $ wrap a
unsafeFacet (Nothing : _) First = Empty
unsafeFacet (_ : as) (Later l) = unsafeFacet as l
unsafeFacet [] _ = error "The provided list isn't long enough to use as a Faceted over the intended parties."
-}


parallel :: forall ls a ps m.
            (KnownSymbols ls)
         => Subset ls ps
         -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m a)  -- Could promote this to PIndexed too, but ergonomics might be worse?
         -> Choreo ps m (Faceted ls '[] a)
parallel ls m = sequenceP (PIndexed body)
  where body :: PIndex ls (Compose (Choreo ps m) (Facet a '[]))
        body mls = Compose $ Facet <$> locally (inSuper ls mls) (m mls)

parallel_ :: forall ls ps m.
             (KnownSymbols ls)
          => Subset ls ps
          -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m ())
          -> Choreo ps m ()
parallel_ ls m = void $ parallel ls m

_parallel :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> m a -> Choreo ps m (Faceted ls '[] a)
_parallel ls m = parallel ls \_ _ -> m


-- | Perform a given choreography for each of several parties, giving each of them a return value that form a new `Faceted`.
fanOut :: (KnownSymbols qs)
       => (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located (q ': rs) a))  -- ^ The body.  -- kinda sketchy that rs might not be a subset of ps...
       -> Choreo ps m (Faceted qs rs a)
fanOut body = sequenceP (PIndexed $ Compose . (Facet <$>) <$> body)

-- | Perform a given choreography for each of several parties; the return values are known to recipients but (possibly) not to the loop-parties.
fanIn :: (KnownSymbols qs, KnownSymbols rs)
       => Subset rs ps  -- ^ The recipients.
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))  -- ^ The body.
       -> Choreo ps m (Located rs (Quire qs a))
fanIn rs body = do (PIndexed x) <- sequenceP (PIndexed $ Compose . (Const <$>) <$> body)
                   rs `congruently` \un -> stackLeaves $ \q -> un refl (getConst $ x q)

scatter :: forall census sender recipients a m.
           (KnownSymbol sender, KnownSymbols recipients, Show a, Read a)
        => Member sender census
        -> Subset recipients census
        -> Located '[sender] (Quire recipients a)
        -> Choreo census m (Faceted recipients '[sender] a)
scatter sender recipients values = fanOut \r ->
    (sender, \un -> un First values `getLeaf` r) *~> inSuper recipients r @@ sender @@ nobody

gather :: forall census recipients senders a dontcare m.
          (KnownSymbols senders, KnownSymbols recipients, Show a, Read a)
       => Subset senders census
       -> Subset recipients census
       -> Faceted senders dontcare a
       -> Choreo census m (Located recipients (Quire senders a))  -- could be Faceted senders recipients instead...
gather senders recipients (PIndexed values) = fanIn recipients \s ->
    (inSuper senders s, getFacet $ values s) ~> recipients

