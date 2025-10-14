---
title: Lax functors describe emergent effects
date: 02/12/2019
---

In this post I'll describe something that's kind of common knowledge among applied category theorists, that when you describe behaviour via a (pseudo)functor to the category of relations, emergent effects are described by the failure of that functor to be strong, ie. to be an actual functor. This is more or less in [Seven Sketches](https://arxiv.org/abs/1803.05316) (Fong and Spivak say "generative effects", which as far as I can tell is an exact synonym for emergent effects), but I'll write it in one place and in my own words.

Suppose we have a domain-specific category $\mathcal C$. It's hopefully monoidal and most likely a hypergraph category, but the basic idea of what I'm saying applies just to the category structure. The morphisms of $\mathcal C$ are open systems of some sort, the objects describe the open boundaries that a system can have, and composition describes coupling two systems together along a common boundary. The standard sledgehammer for building categories like this is [decorated cospans](https://arxiv.org/abs/1502.00872), with [structured cospans](https://arxiv.org/abs/1911.04630) as a closely related new alternative.

Now suppose we define an operation $F : \mathcal C \to \mathbf{Rel}$, which we think of as describing 'behaviour', understood broadly. Every object $x$ is sent to a set $F (x)$ of behaviours that the boundaries can exhibit. Every morphism $f : x \to y$ is set to a subset $F (f) \subseteq F (x) \times F (y)$, where we think of $(a, b) \in F (f)$ as saying that, if we have the system $f$, it is possible to simultaneously observe the behaviour $a$ on its left boundary and $b$ on its right boundary.

Now think about what happens when we couple a pair of systems $f : x \to y$, $g : y \to z$ along their common boundary, yielding the complex system $fg : x \to z$. We now have three sets of behaviours: the behaviours $F (f) \subseteq F (x) \times F (y)$ of the first component, the behaviours $F (g) \subseteq F (y) \times F (z)$ of the second component, and the behaviours $F (fg) \subseteq F (x) \times F (z)$ of the complex.

Under reasonable conditions, it is the case that every pair of behaviours of $f$ and $g$ that are consistent, in the sense that they agree on the behaviour of the common boundary, yield a behaviour of the complex system. (This can certainly fail – I can think of one example involving Nash equilibria of games under a naive composition like adding payoffs – but if it fails you should probably give up and try something different.) The behaviours on the outer boundaries that arise from behaviours of the components that are consistent on the middle boundary is exactly described by the composition in the category $\mathbf{Rel}$:

$$ F (f) F (g) = \{ (a, c) \mid (a, b) \in F (f) \text{ and } (b, c) \in F (g) \text{ for some } b \in F (y) \} $$

So the reasonable condition we imposed on behaviours is that

$$ F (f) F (g) \subseteq F (fg) $$

which is the condition that F is a *lax (pseudo)functor* (thinking of $\mathbf{Rel}$ as a fairly boring 2-category).

If we always have $F (f) F (g) = F (fg)$, then $F$ is a functor. This condition says that every possible behaviour of $fg$ arises in this way from behaviours of $f$ and $g$ that are consistent along $y$. This is a very strong requirement in practice, and generally fails. (An example of it failing is [reachability in Petri nets](https://arxiv.org/abs/1808.05415); I have an in-progress example involving smooth optimisation.)

Failure of a lax functor to be a functor is caused by *emergent effects*, that is to say, behaviours of a complex system that do not come from possible behaviours of the components. Under this very general setup, we can give a precise definition of an emergent behaviour of a complex system $fg$: it is as an element of the set

$$ F (fg) \setminus F (f) F (g) $$

This much is what I expect most applied category theorists to know, so I'm writing for everyone else. Now I'm going to end with a difficult challenge for ACT.

It's a good achievement to have such a clean definition of emergent effects, but I am worried that we can't follow it up. Instead of saying "huh, my class of systems exhibits emergent effects" and moving on, can we say more? What I would like is to be able to associate, to a given lax (probably monoidal, hypergraph) pseudofunctor into $\mathbf{Rel}$ (or variants such as $\mathbf{LinRel}$), some kind of object that encodes some useful knowledge about its failure to be a functor. Ideally, something that makes it easier to calculate (or otherwise do something with) $F (fg)$ if we know $F (f)$ and $F (g)$. Algebraic topology has a bunch of machinery that seems like it might be able to do this sort of thing (especially if we start imposing some reasonable extra conditions on our category of open systems $\mathcal C$ or on $F$), but I don't understand it well enough.

**Edit**: [Elie Adam's PhD thesis](https://elieadam.com/eadam_PhDThesis.pdf) looks closely related to this, but is built on different foundations (the terms "pseudo" and "lax" do not match in the text). Hopefully one of these years I'll have some time to think about how it fits together.
