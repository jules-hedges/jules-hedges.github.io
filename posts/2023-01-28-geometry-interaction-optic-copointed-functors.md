---
title: Geometry of interaction is the optic for copointed functors
date: 28/01/2023
---

[Geometry of Interaction](https://en.wikipedia.org/wiki/Geometry_of_interaction) (also known as the [Int-construction](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/joyal-street-verity-traced-monoidal-categories.pdf)) is an important construction in category theory that shows up the semantics of concurrency. It's also a contender for my favourite thing in category theory. It's one member of a whole zoo of things that look kinda like lenses but are a bit different. Back around 2017 when I was writing [The game semantics of game theory](https://arxiv.org/abs/1904.11287) I asked myself whether Int was an optic, and concluded it probably wasn't, but recently I was thinking about the question with my PhD student Riu Rodríguez Sakamoto and we realised that it actually is. In this post I'll sketch the construction in the terms of Haskell's Lens library. None of the proofs are done yet, and all of this ought to work in a general categorical setting, but what I wrote here demonstrably works by testing. The source code can be found [here](https://github.com/CyberCat-Institute/cybercat-core/blob/master/src/Cybercat/Sketches/IntLens.hs).

In Haskell, Int-morphisms are concretely given by
```haskell
type ConcreteInt s t a b = (s, b) -> (a, t)
```
or equivalently as a pair of functions `s -> b -> a` and `s -> b -> t`. That is, it's like a lens except that the getter has an extra input. This "breaks causality", and composing Int-morphisms requires using non-well-founded recursion to cut the knot:
```haskell
(>>>) :: ConcreteInt s t a b -> ConcreteInt a b p q -> ConcreteInt s t p q
(f >>> g) (s, q) = let (a, t) = f (s, b)
                       (p, b) = g (a, q)
                    in (p, t)
```

There's an easy embedding of lenses into Int, namely by having the getter just ignore its extra input. In [The game semantics of game theory](https://arxiv.org/abs/1904.11287) I proved this embedding is functorial: composing 2 lenses and then embedding into Int is the same as embedding both into Int and then composing them there. In that case the non-well-founded recursion short-circuits and everything terminates.

(It's worth mentioning that the definition of Int works for any traced monoidal category, where it satisfies the universal property of being the free compact closed category. In the monoidal case lenses turn into coend optics, and [it was proven](https://raw.githubusercontent.com/mroman42/optic-int-construction/master/opticint.pdf) by Elena di Lavore and Mario Román that this functoriality property holds in this fully general setting. But Haskell types are nonlinear, so I'll stick to the easier more specific case in this post.)

If you have a van Laarhoven-encoded lens, of type `forall f. (Functor f) => (a -> f b) -> (s -> f t)`, the way you get the getter is by instantiating with the functor `Const a` to get `(a -> a) -> (s -> a)`, while the way you get the setter is to instantiate with the functor `Identity` to get `(a -> b) -> (s -> t)`. We want to rule out the former, so we need some additional constraint on top of `Functor` that is not satisfied by `Const a`. I had the idea to use copointed functors:
```haskell
class (Functor f) => Copointed f where
    extract :: f a -> a
```

(This looks like a lot like the definition of a functor algebra, the difference is that here `a` is a type variable ranging over all types rather than a specific type. It also looks a lot like the `Foldable` class, in which `a` ranges only over monoids.)

The conjecture is that the Int-construction is the optic for `Copointed`:
```haskell
type Int s t a b = forall f. (Copointed f) => (a -> f b) -> (s -> f t)
```

The identity functor is of course copointed:
```haskell
instance Copointed Identity where
    extract = runIdentity
```

But `Const a` is not, since you can't write a function `Const a b -> b`. The functor we're going to use instead is the state monad, given by
```haskell
data State s a = State {runState :: s -> (a, s)}
```

It turns out that `State s` is `Copointed`, but only if you allow non-well-founded recursion to trace out `s`. This is good, since if we're implementing Int we must have to use non-well-founded recursion somewhere.

```haskell
instance Copointed (State s) where
    extract f = let (a, s) = runState f s in a
```

We're going to get the putter in exactly the same way we do with lenses, by instiantiating with `Identity`:
```haskell
intPut :: Int s t a b -> s -> b -> t
intPut f s b = runIdentity (f (const (Identity b)) s)
```

To get the getter, we're going to instead instantiate with `State a`. I was surprised when the thing that ended up working also used non-well-founded recursion here:
```haskell
intGet :: Int s t a b -> s -> b -> a
intGet f s b = let (_, a) = runState (f (State . const . (b,)) s) a
                in a
```

We'd also better be able to embed concrete Int-morphisms (as the lens function does for concrete lenses), and I was even more surprised when this ended up using non-well-founded recursion too:
```haskell
int :: (s -> b -> a) -> (s -> b -> t) -> Int s t a b
int f g k s = let a = f s (extract (k a)) 
               in fmap (g s) (k a)
```

And that's it. The previous 2 functions I wrote by type directed programming and I didn't expect it to work, because I was expecting to need to use non-well-founded recursion in 1 place and not 3 places. So I was extremely surprised when I encoded a pair of basic lenses, composed them and found that the result terminates with the correct answer:
```haskell
g1 (x, y, z) _ = (x, y)
p1 (x, y, z) (x', y') = (x', y', z)
g2 (x, y) _ = x
p2 (x, y) x' = (x', y)
```

(It turns out that the wildcard patterns in the getters are crucial: if you force it to match a pair, everything goes horribly wrong. This sentence is brought to you by a major panic while finishing up this post.)

```haskell
> intGet (int g1 p1 . int g2 p2) (69, 420, "nice") undefined
69
> intPut (int g1 p1 . int g2 p2) (69, 420, "nice") "blaze it"
("blaze it",420,"nice")
```

Perhaps I'll write a followup post exploring the kinds of shenanigans you can get up to using the Int-construction, like building optics that behave like concurrent processes that can fail to terminate by deadlocking.
