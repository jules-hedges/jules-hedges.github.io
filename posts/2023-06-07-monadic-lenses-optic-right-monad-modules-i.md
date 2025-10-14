---
title: Monadic lenses are the optic for right monad modules I
date: 07/06/2023
---

I figured out encoding of 2 more lens-like constructions in the "van Laarhoven" (functor quantification) style used by Haskell's [`Control.Lens`](https://hackage.haskell.org/package/lens-5.2.2/docs/Control-Lens.html). The first is monadic lenses, which allow your backwards pass to have side effects from a monad, and the second is the far more subtle monadic optics, which allow effects on both the forwards and backwards passes. I was originally surprised to find neither of these in the monolith-with-included-batteries that is `Control.Lens`. To summarise my findings:

- Monadic lenses are the optic for right modules of the monad
- Monadic optics are the optic for right modules of the monad that additionally the monad distributes over

Source code for this post can be found [here](https://github.com/CyberCat-Institute/cybercat-core/blob/master/src/Cybercat/Sketches/MonadicLens.hs). As usual I didn't prove correctness of either of these, they're only conjectures that seem to check out in code. In this post I'll cover the first one, and then part II will be about the second, which is a lot more complicated. Let's dive in.

In this post we're working with a fixed monad, $m$. A **monadic** lens from $(s, t)$ to $(a, b)$ is, by definition, a pair of a "forwards pass" or "get" function $s \to a$ and a "backwards pass" or "put" function $s \times b \to m t$. This definition first appeared in section 2.3 of the paper [Reflections on Monadic Lenses](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/mlenses.pdf).[^1] The reason that the monad appears only on the backwards pass is that if we naively add it to the forwards pass (even if we also drop it from the backwards pass!) then lens composition completely fails to be associative. In the second half of this post we discuss how this is overcome.

[^1]: Editor's note: Jeremy Gibbons commented that the definition appears slightly earlier in [this paper](https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/entangled.pdf) and also independently in [this paper](https://dl.acm.org/doi/10.1145/2543728.2543737).

Let's open up the [bonnet](https://gaystepdadblog.wordpress.com/wp-content/uploads/2017/02/lets-dothings-together.png) and remind ourselves how van Laarhoven lenses work. An ordinary lens $(s, t) \to (a, b)$, which is concretely a pair of a forwards pass $s \to a$ and a backwards pass $s \times b \to t$, is encoded via quantification over all functors:
```haskell
type Lens s t a b = forall f. (Functor f) => (a -> f b) -> (s -> f t)
```

In order to extract the forwards pass from this, we instantiate $f$ to be the functor that is constant at $a$, leaving us with $(a \to a) \to (s \to a)$, and then applying the identity function. To extract the backwards pass, we instead instantiate $f$ to be the identity functor, leaving us with $(a \to b) \to (s \to t)$, and then precomposing with the const function $b \to a \to b$ to get a function $b \to s \to t$. 

Going the other way, in order to turn a concrete lens into a van Laarhoven lens we must produce an $f t$ from a continuation $a \to f b$ and an $s$ knowing only that $f$ is a functor. Using our $s$, we apply the forwards pass to get an $a$ followed by the continuation to get an $f b$. Then we partially apply $s$ to the backwards pass to get a function $b \to t$, which we fmap over our $f b$ to get an $f t$, which is what we needed.

So, in order to encode monadic lenses, we need to add a constraint that allows us to keep using the constant functor but prevents us using the identity functor. Instead of identity, in order to obtain the monadic backwards pass we would like to set $f$ to be $m$ itself, giving us $(a \to m b) \to (s \to m t)$. From there, we can get a function $s \to b \to m t$ using the constant pure function $b \to a \to m b$.

Going the other way, let's think about what we would need to know about $f$ in order to make an $(a \to f b) \to (s \to f t)$ out of a monadic lens. Given $s$ we can apply the forwards pass to get an $a$ and then the continuation to get an $f b$, the same as before. Now if we fmap over the partially applied backwards pass $b \to m t$ we can get an $f (m t)$. But we wanted an $f t$, so what we need to know about $f$ is that we have a way to do $f (m t) \to f t$.

It turns out that a functor that allows you to do this (for all $t$) is exactly what is called a **right $m$-module**. And everything lines up: constant functors are always right modules trivially, every monad is a right module of itself by the monad multiplication, and the identity functor is not a right module unless you can escape the monad.

If a monad is just a monoid in the category of endofunctors, then a right module is a right action of that monoid... what's the problem? Right modules of monads do occasionally crop up in functional programming, see for example [this paper](https://research-information.bris.ac.uk/ws/portalfiles/portal/87127912/Nicolas_Wu_String_Diagrams_for_Free_Monads.pdf).

Here's the key bits of the [code](https://github.com/CyberCat-Institute/cybercat-core/blob/master/src/Cybercat/Sketches/MonadicLens.hs):
```haskell
class RightModule m f where
    act :: f (m a) -> f a
 
instance (Monad m) => RightModule m m where
    act = join
 
instance RightModule m (Const a) where
    act = Const . getConst
 
type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b
 
monadicGet :: MonadicLens m s t a b -> s -> a
monadicGet l s = getConst (l (Const) s)
 
monadicPut :: (Monad m) => MonadicLens m s t a b -> s -> b -> m t
monadicPut l s b = l (const (return b)) s
 
monadicLens :: (Monad m) => (s -> a) -> (s -> b -> m t) -> MonadicLens m s t a b
monadicLens g p k s = act (fmap (p s) (k (g s)))
```

And that's it! I had to switch on incoherent instances to make this compile, but I think that's just Haskell being Haskell – I think it's unable to figure out that the two instances can never overlap because `Const a` is not a monad.

This was the easy part. Stay tuned for part 2 where we'll also add the monad to the forwards pass.
