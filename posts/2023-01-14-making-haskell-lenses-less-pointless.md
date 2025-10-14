---
title: Making Haskell lenses less pointless
date: 14/01/2023
---

Fair warning: this post assumes some familiarity with Haskell's `Control.Lens` library. Source code for this post can be found [here](https://github.com/CyberCat-Institute/cybercat-core/blob/master/src/Cybercat/Sketches/LensValues.hs).

Recently I've been working on a major rewrite of the [open game engine](https://github.com/CyberCat-Institute/open-game-engine), based on some newer theoretical ideas such as [this paper](https://arxiv.org/abs/2206.12338). A major objective is to no longer require a custom DSL, removing the dependency on Template Haskell and especially the need to maintain a parser. For reasons that are brutally obvious to anyone who's ever programmed with open games, it's a hard requirement to have a syntax based on name-binding: working with point-free combinators is not humanly possible at this scale. This post is a spin-off from that work, explaining how to use Haskell's [Lens library](https://hackage.haskell.org/package/lens-5.2) in a name-binding rather than point-free style.

The key idea is that if you have a lens of type `Lens s t a b`, and you want to treat it as though it's a function, then the corresponding notion of "value" is something of type `(s, t -> r)`. That is, it's an input for the lens' getter, together with a continuation from the output of the setter. Given a lens and such a value-continuation pair, you can get an "output value" of type `(a, b -> r)`.

It turns out that this type is already exported by `Control.Lens`, where it's called
```haskell
data Context s t r = Context (t -> r) s
```

But it will be convenient to rewrite this in an isomorphic van Laarhoven style, in order that we get "lens application" for free. Here's the key definition of this post:
```haskell
type Value f s t r = (s -> f t) -> f r
```

Now we have a type isomorphism
```haskell
(s, t -> r) === forall f. (Functor f) => Value f s t r
```
and we can also compose a `Value` with a `Lens`, using ordinary function application, to get a new `Value`.

(It transpires that putting off the quantification over `f` until later works much better, for the same reason that `Lens` is defined in terms of `LensLike`.)

Now a lens is equivalently a function that returns a `Value`:
```haskell
Lens s t a b === forall f. (Functor f) => s -> Value f a b t
```

We're going to build things like the right hand side with our name-binding style, so it's most useful to be able to go from right to left:
```haskell
buildLens :: (s -> Value f a b t) -> LensLike f s t a b
buildLens f k s = f s k
```

If we have an ordinary value, we can upgrade it to a `Value` that has the identity function for its continuation:
```haskell
value :: s -> Value f s t t
value s k = k s
```

And that's all we need! Let's test it out by importing [`Control.Lens.Tutorial`](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html) to get some examples to play with. A recurring example in that tutorial is
```haskell
atpx :: Traversal' Atom Double
atpx = atoms . traverse . point . x
```

Let's rewrite it in a name-binding style:
```haskell
atpx :: Traversal' Molecule Double
atpx = buildLens $ \molecule -> let atom = value molecule . atoms
                                    traversal = atom . traverse
                                    xy = traversal . point
                                 in xy . x
```

Here you can see all the pieces in use. We lambda-abstract to bring a `Molecule` into scope, which we then need to turn into a `Value` using our value function. Now we can apply lenses to it using function application. Because of how we postponed the quantification over `f`, by van Laarhoven magic it works for everything in the `Lens` type hierarchy, demonstrated here with `Traversal`s.

One additional upside of this style is we can write local type signatures, although we need `PartialTypeSignatures` because the quantification over `f` can't be locally solved, and then by default ghc spams us with its solutions for the holes:
```haskell
atpx :: Traversal' Molecule Double
atpx = buildLens $ \molecule -> let atom :: Value _ [Atom] [Atom] Molecule
                                    atom = value molecule . atoms
                                    traversal :: Value _ Atom Atom Molecule
                                    traversal = atom . traverse
                                    xy :: Value _ Point Point Molecule 
                                    xy = traversal . point
                                 in xy . x
```

I doubt I'm going to convince anyone to use this style to replace such a simple "linear pipeline" of lenses, but when it comes into its own is when the pipeline splits and merges, something that's ubiquitous in my research. To demonstrate this, let's write a lens that computes the average of two `Double`s, and if the average is updated then it modifies both values by an equal amount in order to get the desired average:
```haskell
average :: Lens' (Double, Double) Double
average = lens (\(x, y) -> f x y) 
               (\(x, y) a -> let a' = f x y
                              in (x - a' + a, y - a' + a))
  where f x y = (x + y)/2
```

Now if we have a pair of `Atom`s, we can write a lens to their average x-coordinate like this:
```haskell
averageX :: Lens' (Atom, Atom) Double
averageX = ((point . x) `alongside` (point . x)) . average
```

The `alongside` operator, exported by `Control.Lens`, implements the tensor product of lenses, focussing onto two things at once. This is still feasible to write point-free, but it becomes humanly impossible surprisingly quickly on bigger examples.

In order to write this with name-binding we need one more thing for our library: a way of turning a pair of `Value`s into a `Value` of pairs. Its implementation is pretty mysterious and almost exactly the same as the implementation of `alongside`:
```haskell
(/\) :: Value (AlongsideLeft f b') a b t 
     -> Value (AlongsideRight f t) a' b' t' 
     -> Value f (a, a') (b, b') (t, t')
(l /\ m) k = getAlongsideRight (m (\a' -> AlongsideRight 
             (getAlongsideLeft (l (\a -> AlongsideLeft (k (a, a')))))))
```

Here `AlongsideLeft` and `AlongsideRight` are helper functors which are exported from `Control.Lens.Internal.Getter`. Fortunately you can cheerfully use this operator without understanding its type or implementation. (`Control.Lens` exports so many infix operators, I had to get slightly creative thinking of a name for this one.)

Armed with this operator, we can rewrite `averageX` in name-binding style:
```haskell
averageX :: Lens' (Atom, Atom) Double
averageX = buildLens $ \(a1, a2) -> let pos1 = value a1 . point
                                        pos2 = value a2 . point
                                        x1 = pos1 . x
                                        x2 = pos2 . x
                                     in (x1 /\ x2) . average
```

(Who thought it was a good idea to give `(.)` the highest possible operator precedence in Prelude?)

And that's all I've got. I'm still on the fence about whether to build `cybercat-core` on top of `Control.Lens` or on top of linear lenses `s -> (a, b -> t)`: the former allows van Laarhoven subtyping magic but it's not clear whether I actually need that for my purposes, while the latter gives much easier type errors and is generally much easier to work with. 
