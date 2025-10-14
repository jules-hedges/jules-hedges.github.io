---
title: Monadic lenses are the optic for right monad modules III
date: 28/06/2023
---

This post picks up where the [exposition in the first installment](/posts/2023-06-07-monadic-lenses-optic-right-monad-modules-i.html) left off, before the sideline [development in the second installment](/posts/2023-06-24-monadic-lenses-optic-right-monad-modules-ii.html). I never intended to make a blog post in [sonata form](https://en.wikipedia.org/wiki/Sonata_form), but there we are. Code for this post can be found [here](https://github.com/CyberCat-Institute/cybercat-core/blob/master/src/Cybercat/Sketches/MonadicOptic.hs).

If we'd like both our forwards and backwards passes to live in our monad then this is the simplest case that there is no possible concrete description, without using any quantification. If we naively put the monad on both the forwards and backwards passes, so we have $s \to m a$ and $s \times b \to m t$, it even looks like we can compose them, but it's a trap: this composition law completely fails to be associative for virtually all monads of interest. [I fell into this exact trap in my PhD thesis](/posts/2018-01-16-towards-compositional-game-theory.html), which also flew by my examiners until it was caught by my former PhD student Joe Bolt.

The solution, which is the main topic of [Mitchell Riley's important paper](https://arxiv.org/abs/1809.00738), is to use coend optics in the kleisli category. Using this to repair my thesis eventually led to the paper [Bayesian Open Games](https://arxiv.org/abs/1910.03656), which went on to be [by far the most useful version of open games in practice](https://20squares.xyz/). The motivation behind this post is to use my "[less pointless lenses](/posts/2023-01-14-making-haskell-lenses-less-pointless.html)" trick with the "[diegetic](https://arxiv.org/abs/2206.12338)" variant of Bayesian open games in order to get a domain specific language written directly in native Haskell rather than compiled via templates, which would have several practical advantages. This is one of several steps needed to get to that point.

The way to define a lens $(s, t) \to (a, b)$ for which both the forwards and backwards passes live in the monad $m$ is
```haskell
type ExistentialKleisliOptic m s t a b = exists z. (s -> m (a, z), z -> b -> m t)
```

These don't have an agreed name, but I usually call them "monadic optics" or "kleisli optics". The intuitive idea is that in the non-cartesian world of the monad, the existentially bound variable $z$ can 'entangle' the forwards and backwards passes. In Haskell this is easiest to encode as a GADT:
```haskell
data ExistentialKleisliOptic m s t a b where
  ExistentialKleisliOptic :: (s -> m (a, z)) -> (z -> b -> m t) -> ExistentialKleisliOptic m s t a b
```

The reason this encoding is pointless is that it can't be rewritten in the form of a function from $s$ to something, unlike van Laarhoven lenses which can.

Until now I've always worked directly with the existential encoding. For example, we can define composition directly:
```haskell
(>>>) :: (Monad m)
      => ExistentialKleisliOptic m s t a b
      -> ExistentialKleisliOptic m a b o p
      -> ExistentialKleisliOptic m s t o p
(ExistentialKleisliOptic g1 p1) >>> (ExistentialKleisliOptic g2 p2)
  = ExistentialKleisliOptic (\s -> do (a, z1) <- g1 s
                                      (o, z2) <- g2 a
                                      pure (o, (z1, z2)))
                            (\(z1, z2) p -> do b <- p2 z2 p
                                               p1 z1 b)
```

The question is how to encode them in van Laarhoven style. Unlike in [the first post](/posts/2023-06-07-monadic-lenses-optic-right-monad-modules-i.html) for monadic lenses, in this post I'm going to pull the solution from my hat because I don't understand it well enough to guide the reader through inventing it. To encode kleisli optics as a van Laarhoven lens, we need two constraints on our functor $f$:


- Just as for monadic lenses, we need $f$ to be a right module of $m$, that is, we need a natural transformation $f (m a) \to f a$ compatible with the structure of $m$
- We need a distributive law of $m$ over $f$, that is, a natural transformation $m (f a) \to f (m a)$ compatible with the structure of $m$

These either should or shouldn't interact with the structure that was the topic of [the previous post](/posts/2023-06-24-monadic-lenses-optic-right-monad-modules-ii.html), a distributive bimodule – I don't know yet, but the code works the same either way.

Recall that we have defined right modules and distributive laws:
```haskell
class RightModule m f where
    act :: f (m a) -> f a
 
class Distributive m f where
    distribute :: m (f a) -> f (m a)
```

We define kleisli optics in van Laarhoven style as the optic for these constraints:
```haskell
type KleisliOptic m s t a b = forall f.
    (Functor f, RightModule m f, Distributive m f)
    => LensLike f s t a b
```

We need to define the equivalence between the existential and van Laarhoven representations. To go from existential to vL we work with these constraints polymorphically. To go the other way, we need to come up with a specific distributive right module that does the trick.

Ok, let's walk through how to convert from an existential kleisli optic to a van Laarhoven one, by type directed programming. Our existential keisli optic unpacks to a pair of a get map `g :: s -> m (a, z)` and a put map `p :: z -> b -> m t`, where `z` is some existentially bound type. We also have a continuation `k :: a -> f b` where `f` is some functor that is equipped with a right action `act :: f (m x) -> f x` and a distributive law `distribute :: m (f x) -> f (m x)`. We also have an input `s`, and our goal is to produce an `f t`.

Let's immediately make the guess that the last thing we do is to apply the action, which means instead we need to produce an `f (m t)`.

Our next thought might be that the next-last thing to do is to apply the distributive law (which means, secretly, we're ending with the left action given by the distributive law and right action). That would mean that we need to make an `m (f t)`. It turns out though that we can't get there. If we now go into do-notation, we can get `(a, z)` from `g s`. But then from the `a` we get an `f b`. But then `fmap (p z)` has type `f b -> f (m t)`, and we can no longer get back into the monad – we need to use the distributive law on the other side. What we need to do is thread the residual `z` past the distributive law in a more subtle way.

Let's go from the other end. Within do-notation we can get `(a, z)` from `g s`. Now we can get `k a :: f b`. Since we have `z`, we can insert it using `fmap (z, ) (k a) :: f (z, b)`. Stepping back out of the monad, this gives us a subterm of type `m (f (b, z))`,
```haskell
do (a, z) <- g s
   pure (fmap (, z) (k a))
```

Let's apply the distributive law to this, so it becomes of type `f (m (z, b))`. We're still aiming for `f (m t)` so we can end with the action, and we can get there with `fmap` if we can make a function of type `m (z, b) -> m t`. And we can indeed do that – it's basically the kleisli extension of the put map `p :: z -> b -> m t`. The slick way to write it is as an operator section `(>>= (uncurry p))`.

And that's it! The function we built is this:
```haskell
kleisliOptic :: (Monad m) => ExistentialKleisliOptic m s t a b -> KleisliOptic m s t a b
kleisliOptic (ExistentialKleisliOptic g p) k s
  = act $ fmap (>>= (uncurry p)) $ distribute $ do (a, z) <- g s
                                                   pure (fmap (z, ) (k a))
```

This *works*. We can encode existential optics into van Laarhoven style in this way, all of the `Control.Lens` machinery works for it and everything seems to do the right thing. But to be sure, we'd like to convert the other way so we can ideally prove that the two types are isomorphic.

Going the other way is quite subtle. Up until a nearly-finished draft of this post, I had a different solution – a certain functor that carried around its own exstentially-bound type – that failed the vibe check as I was writing it up, so I gave it some more testing and found out that it was broken.

The first bit of inspiration for me was the trick that's needed to convert directly from a van Laarhoven lens to a linear one, ie. `Lens s t a b -> s -> (a, b -> t)`. The trick is to define the monomial functor type `Monomial a b x = (a, b -> x)`. Instantiating the lens `(a -> f b) -> (s -> f t)` to that monomial gives it the type `(a -> (a, b -> b)) -> (s -> (a, b -> t))`, at which point we can apply the obvious input `\a -> (a, id)` (which is actually the identity linear lens of that type!) to get what we need.

The second half of the inspiration was knowing that linear lenses can be encoded existentially: we take the residual `z` to be `b -> t`, so that the forwards pass `s -> (a, z)` is exactly the linear lens, and the backwards pass `z -> b -> t` is just function application. This fact was recorded in [a blog post](https://www.brunogavranovic.com/posts/2022-02-10-optics-vs-lenses-operationally.html) by Bruno Gavranović, which also pointed out that it's secretly what is happening in Conal Elliott's paper [The simple essence of automatic differentiation](http://conal.net/papers/essence-of-ad/).

In order to make this work for kleisli optics, it turns out that we need to modify the definition of the monomial functor by sticking the monad in there *twice*:
```haskell
data Monomial m a b x = Monomial {runMonomial :: m (a, b -> m x)}
  deriving (Functor)
```

We can get the Functor instance automagically if we switch on `DeriveFunctor`. It's easy to make this thing an instance of both `RightModule m` and `Distributive m`, and we find out that we really do need both appearances of the monad in the type to make both of them work:
```haskell
instance (Monad m) => RightModule m (Monomial m a b) where
  act (Monomial af) = Monomial $ do (a, f) <- af
                                    pure (a, join . f)
 
instance (Monad m) => Distributive m (Monomial m a b) where
  distribute maf = Monomial $ do Monomial af <- maf
                                 (a, f) <- af
                                 pure (a, pure . f)
```

Now, given a `KleisliOptic m s t a b`, we can instantiate the functor to get a function `(a -> m (a, b -> m b)) -> (s -> m (a, b -> m t))`. Now we can get an existential kleisli optic with residual type `z = b -> m t`, whose backwards pass is just function application:
```haskell
unKleisliOptic :: (Monad m) => KleisliOptic m s t a b -> ExistentialKleisliOptic m s t a b
unKleisliOptic l = ExistentialKleisliOptic (runMonomial . l (\a -> Monomial (pure (a, pure)))) ($)
```

I'm a little bit suspicious about this, because linear representations of kleisli optics aren't properly understood yet. The type `s -> m (a, b -> m t)` appears in section 5 of [The compiler forest](https://homepages.inf.ed.ac.uk/gdp/publications/compiler-forest.pdf) by Budiu, Galenson and Plotkin. This type *shouldn't* work, because linear lenses work for monoidal closed categories, and kleisli categories are (infuriatingly) not monoidal closed. The obvious choice for the internal function type in a kleisli category would be $x \to m y$ but the curry isomorphism should be $x \times y \to m z \cong x \to m (y \to m z)$, which is not the case. Yet, this type seems like it should work anyway, and we seem to be secretly using that fact to build our equivalence. So this still needs looking into.[^1]

[^1]: Editor's note: I later came back to this question in [this post](/posts/2024-04-12-enriched-closed-lenses.html).
