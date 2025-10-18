---
title: Symmetric polymorphic lenses (maybe)
date: 16/10/2019
---

Whereas ordinary directed lenses go from a source to a view, [symmetric lenses](https://www.cis.upenn.edu/~bcpierce/papers/symmetric.pdf) relate two different views of an internal source. I've been thinking about how to smoosh together symmetric lenses with [polymorphic lenses](http://comonad.com/reader/2012/mirrored-lenses/), which are the sort used in functional programming. My actual goal is to invent "symmetric open games" (without relying on non-well-founded recursion like I did in [this paper](https://arxiv.org/abs/1904.11287)). But with the right definition, I think symmetric polymorphic lenses could be a useful tool to programmers as well.

Normally when I'm playing with an idea like this I mock it up in Haskell and play around with it to check it works, before committing to too much hard work on the proofs. But this would be very difficult (and maybe impossible) to write in Haskell, so the idea of this post is to give enough detail that some folks could try it in Agda or Idris, and give me feedback on whether it feels like something useful. (In the meantime, I will try an extremely fast and loose Haskell version by pretending that pullbacks and pushouts are products and coproducts, and see what happens.)

What I'll write about here is actually symmetric [bimorphic lenses](/posts/2018-08-16-lenses-for-philosophers.html), the word I made up for when the lens laws don't even typecheck. In what is now a fine tradition, I'll figure out how the lens laws come into it later. (I have a nasty feeling that the lens laws might be what kills this idea.)

Rather than starting from the original concrete definition of symmetric lenses, I'm starting from their characterisation as [spans of directed lenses](https://web.science.mq.edu.au/~mike/papers/78.pdf) (modulo a hack, because the category of lenses doesn't quite have pullbacks).

The other ingredient is the fact that the category of bimorphic lenses embeds canonically into the [category of containers](https://people.cs.nott.ac.uk/psztxa/publ/fossacs03.pdf). (This was first pointed out to me by [Fred Nordvall Forsberg](https://personal.cis.strath.ac.uk/fredrik.nordvall-forsberg/).) Morphisms of containers can be viewed as dependently typed lenses, as in [this paper](https://arxiv.org/abs/1908.02202). The category of containers has extremely nice properties: it is complete, cocomplete and locally cartesian closed, making it a model of a big chunk of type theory.

With this, I can give my proposed definition in a maximally slick way: a symmetric bimorphic lens is a span of container morphisms whose feet are in the image of the category of lenses (or equivalently: whose feet have only constant type dependence).

Here's an unpacked definition. A symmetric bimorphic lens between $(S, T)$ and $(A, B)$ consists of 6 things:

- A type $X$ (thought of as the type of initial sources)
- A type $Y$ dependent on $X$ (thought of as the types of updated sources)
- Left-view and right-view functions $X \to S$ and $X \to A$
- Left-update and right-update functions $(x : X) \to T \to Y (x)$ and $(x : X) \to B \to Y (x)$

I think that the types really should be bound by an existential quantifier, and not by a $\Sigma$ — in category theory I think of them as bound by a coend. But probably nothing will go seriously wrong if it's implemented as a $\Sigma$ type.

The remaining thing is how to compose them. Suppose we have symmetric lenses between $(S, T)$ and $(A, B)$ with internal state $(X, Y)$, and another between $(A, B)$ and $(P, Q)$ with internal state $(V, W)$. Then:

- The type of initial sources is $X \times_A V$, in other words, the pairs of initial sources which agree on their common view
- The updated sources indexed by $(x, v) : X \times_A V$ is the pushout $Y (x) +_B W (v)$

I could try to write down the new view and update maps, but if you're hacking this up I expect it's easier to just follow the types.

**Update**: After writing a simple Haskell implementation (see [this thread](https://twitter.com/_julesh_/status/1184569609300664320)), I spotted the fairly big problem with existential quantification: Since you can't get at the type of initial states, you can never actually make the lens do anything. So, $\Sigma$ types it probably is then.
