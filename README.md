# MultiChor



MultiChor is a library for writing choreographic programs in Haskell.

That means you write _one_ program, a _"choreography"_ which seamlessly describes the actions of _many_ communicating machines;
these participants can be native Haskell threads, or various humans' laptops communicating over HTTPS, or anything in between.
Each of these "endpoints" will "project" their behavior out of the choreography.

Choreographies aren't just easier to write than distributed programs, they're automatically deadlock-free!

MultiChor uses some of the same conventions and internal machinery as [HasChor](https://github.com/gshen42/HasChor),
but the API is incompatible and can express more kinds of choreographic behavior.

- The heart of the MultiChor library is a choreography monad `Choreo ps m a`.
  - `ps` is a type-level list of parties participating in the choreography,
  - `m` is an monad used to write _local_ actions those parties can take,
  - `a` is the returned value, typically this will be a `Located` or `Faceted` value as described below.
- MultiChor is an _embedded_ DSL, as interoperable with the rest of the Haskell ecosystem as any other monad.
  In particular, MultiChor gets recursion, polymorphism, and location-polymorphism "for free" as features of Haskell!
- MultiChor uses multicast and multiply-located values, as introduced in [We Know I Know You Know](https://arxiv.org/abs/2403.05417).
  - A value of type `Located ls a` is a single `a` known to all the parties listed in `ls`.
    In a well-typed choreography, other parties, who may not know the `a`, will never attempt to use it.
  - In the expression `(s, v) ~> rs`, a sender `s` sends the value `v` to _all_ of the recipients in `rs`, resulting in a `Located rs v`.
  - For easy branching in choreographies, the primitives `enclave` and `naked` combine to form `cond`.
    ``(parties, guard) `cond` (\g -> c)`` unwraps a `Located parties g` for use as a naked `g` in the conditional choreography `c`.
- Safe handing of parties, party-sets, and located values is enforced using [Ghosts of Departed Proofs](https://hackage.haskell.org/package/gdp).
  In particular, instead of specifying the party `"alice"` in a choreography as a `String` or a `Proxy "alice"`,
  they're specified by a `Proof` that the type-level `"alice"` is present in the choreography and has access to the relevant values.
  MultiChor provides utilities to write these proofs compactly.
- The novel choreographic language feature "TODO polymorphism" allows you to write choreographies
  that are polymorphic with respect to the _number of parties_ in a polymorphic party-set.
  This is trivial if they're passively receiving values; new primitives allow them to actively communicate.
  - `fanOut` lets a single party send different values (of the same type `a`) to a list of parties `rs`, resulting in a `Faceted rs a`.
  - `fanIn` lets a list of parties `ss` each send a value to the same parties `rs`, resulting in a `Located rs [a]`.
  - A `x :: Faceted ps a` represents _distinct_ `a`s known to each of `ps` _respectively_.
- MultiChor allows parallel behavior of many parties to be concisely expressed.
  - `parallel` lets many parties perform local monadic actions in parallel using their `Located` and `Faceted` values;
    the return is `Faceted`.
  - `replicatively` lets many parties perform _the same pure computation_ in parallel, using only their `Located` values;
    the return is `Located`.


## Examples

Many example choreographies are presented in the [examples](examples) directory.

