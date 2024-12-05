# MultiChor

MultiChor is a library for writing choreographic programs in Haskell.

That means you write _one_ program, a _"choreography"_ which seamlessly describes the actions of _many_ communicating machines;
these participants can be native Haskell threads, or various humans' laptops communicating over HTTPS, or anything in between.
Each of these "endpoints" will "project" their behavior out of the choreography.

Choreographies aren't just easier to write than distributed programs, they're automatically deadlock-free!

MultiChor uses some of the same conventions and internal machinery as [HasChor](https://hackage.haskell.org/package/HasChor),
but the API is incompatible and can express more kinds of choreographic behavior.

- The heart of the MultiChor library is a choreography monad `Choreo ps m a`.
  - `ps` is a type-level list of parties participating in the choreography,
  - `m` is an monad used to write _local_ actions those parties can take (often this is simply `IO`),
  - `a` is the returned value, typically this will be a `Located` or `Faceted` value as described below.
- MultiChor is an _embedded_ DSL, as interoperable with the rest of the Haskell ecosystem as any other monad.
  In particular, MultiChor gets recursion, polymorphism, and location-polymorphism "for free" as features of Haskell!
- MultiChor uses enclaves, and multiply-located values to achieve excellent expressivity and efficient Knowledge of Choice management.
  - A value of type `Located ls a` is a single `a` known to all the parties listed in `ls`.
    In a well-typed choreography, other parties, who may not know the `a`, will never attempt to use it.
  - In the expression `(s, v) ~> rs`, a sender `s` sends the value `v` to _all_ of the recipients in `rs`, resulting in a `Located rs v`.
- Safe handing of parties, party-sets, and located values is enforced using term-level proof objects.
  In particular, instead of specifying the party `"alice"` in a choreography as a `String` or a `Proxy "alice"`,
  they're specified by a "proof" that the type-level `"alice"` is present in the choreography and has access to the relevant values.
  MultiChor provides utilities to write these proofs compactly.
- In addition to location polymorphism, MultiChor allows you to write choreographies
  that are polymorphic with respect to the _number of parties_ in a polymorphic party-set.
  This is trivial if they're passively receiving values; new functions allow them to actively communicate:
  - `fanOut` lets a single party send different values (of the same type `a`) to a list of parties `rs`, resulting in a `Faceted rs a`.
  - `fanIn` lets a list of parties `ss` each send a value to the same parties `rs`, resulting in a `Located rs (Quire ss a)`.
  - A `x :: Faceted ps qs a` represents _distinct_ `a`s known to each of `ps` _respectively_; the parties in `qs` know _all_ the `a`s.
- MultiChor allows parallel behavior of many parties to be concisely expressed.
  - `parallel` lets many parties perform local monadic actions in parallel using their `Located` and `Faceted` values;
    the return is `Faceted`.
  - `congruently` lets many parties perform _the same pure computation_ in parallel, using only their `Located` values;
    the return is `Located`.

## Examples

Consider the choreography `game`:

```haskell
{- A simple black-jack-style game. The dealer gives everyone a card, face up. Each player may
 - request a second card. Then the dealer reveals one more card that applies to everyone. Each
 - player individually wins if the sum of their cards (modulo 21) is greater than 19.  -}
game :: forall players m. (KnownSymbols players) => Choreo ("dealer" ': players) (CLI m) ()
game = do
  let players = consSuper (refl @players)
      dealer = listedFirst @"dealer"
      everyone = refl @("dealer" ': players)
  hand1 <-
    ( fanIn everyone \(player :: Member player players) -> do
        card1 <- locally dealer (\_ -> getInput ("Enter random card for " ++ toLocTm player))
        (dealer, card1) ~> everyone
      )
      >>= naked everyone
  wantsNextCard <- parallel players \_ _ -> do
    putNote $ "All cards on the table: " ++ show hand1
    getInput "I'll ask for another? [True/False]"
  hand2 <- fanOut \(player :: Member player players) ->
    enclave (inSuper players player @@ dealer @@ nobody) do
      let dealer' = listedSecond @"dealer"
      choice <- broadcast (listedFirst @player, localize player wantsNextCard)
      if choice
        then do
          cd2 <- locally dealer' (\_ -> getInput (toLocTm player ++ "'s second card:"))
          card2 <- broadcast (dealer', cd2)
          return [getLeaf hand1 player, card2]
        else return [getLeaf hand1 player]
  tblCrd <- locally dealer (\_ -> getInput "Enter a single card for everyone:")
  tableCard <- (dealer, tblCrd) ~> players
  void $ parallel players \player un -> do
    let hand = un player tableCard : viewFacet un player hand2
    putNote $ "My hand: " ++ show hand
    putOutput "My win result:" $ sum hand > card 19
```

The type signature tells us that this choreography (computation in the `Choreo` monad)
involves a partially-polymorphic list of participants;
the first of whom is specifically `"dealer"` and the rest of whom are represented collectively in the type variable `players`.
All parties can perform computations in a local monad, in this case `CLI m`
(the details of `CLI` are orthogonal to choreographic programming; it's basically just `IO`).
The return type `()` indicates that this choreography doesn't yield any values; it's just a program.
The first thing we do is declare the values `players`, `dealer`, and `everyone`;
these are term-level identifiers for the type level parties, but they're also proofs of subset or membership that
attest that the respective parties are valid to participate in the choreography.
`hand1` has type `Quire players Card`; since these cards are supposed to be dealt face down, they're known to everyone.
To accomplish this, we use a `fanIn` loop in which the dealer selects a card for each player and sends it to everyone.
`wantsNextCard` is the result of a parallel computation (CLI interaction);
it's type is `Faceted players '[] Bool`.
The computation of `hand2` is private between each player and the dealer, so we `fanOut` over the players
and then `enclave` a sub-choreography involving only the given player and `"dealer"`.
The `broadcast` inside the enclave shares that player's value of `wantsNextCard` with everyone _who's present_, which is just `"dealer"`.
At the end, the players each observer if they've won or lost in parallel.

Check the source repository for many more example choreographies.
You can also read the preprint of [_Efficient, Portable, Census-Polymorphic Choreographic Programming_](https://arxiv.org/abs/2412.02107)
for more theoretical discussion of choreographic programming _à la_ MultiChor.
