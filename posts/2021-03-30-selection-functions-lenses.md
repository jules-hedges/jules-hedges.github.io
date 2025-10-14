---
title: Selection functions and lenses
date: 30/03/2021
---

I've been wondering for several years how selection functions and lenses relate to each other, I felt intuitively that there should be some connection – and not just because they both show up in the foundations of game theory. Last night I came up with an answer, which isn't a complete answer but looks like the starting point for a complete answer.

A selection function is a function of type $\mathcal J_R X = (X \to R) \to X$. The canonical example is $\arg\max : (X \to \mathbb R) \to X$, after choosing a single maximiser for every function (say that $X$ is a finite set to avoid worrying about existence of a maximiser). $\mathcal J_R$ is a strong monad, closely related to the continuation monad, with a highly non-obvious monad structure. Combining argmaxen using the monadic bind makes the [backward induction](https://en.wikipedia.org/wiki/Backward_induction) algorithm from game theory fall out of pure category theory – see a string of papers by Martín Escardó and Paulo Oliva, especially [Sequential games and optimal strategies](https://www.eecs.qmul.ac.uk/~pbo/papers/paper032.pdf) for a good introduction.

Meanwhile, the category of lenses $\mathbf{Lens}$ has as objects pairs of sets $(X, R)$, and its morphisms $(X, S) \to (Y, R)$ consist of pairs of functions $v : X \to Y$ and $u : X \times R \to S$. The identity lens on $(X, S)$ consists of an identity function and a projection, and the composition of $(v_1, u_1) : (X, S) \to (Y, R)$ and $(v_2, u_2) : (Y, R) \to (Z, Q)$ consists of $v (x) = v_2 (v_1 (x))$ and $u (x, q) = u_1 (x, u_2 (v_1 (x), q))$. Lenses also play a deep role in the foundations of game theory, via [open games](https://arxiv.org/abs/1603.04641).

What I noticed is that there is a functor $\mathcal J : \mathbf{Lens} \to \mathbf{Set}$, taking the object $(X, R)$ to the set of selection functions $\mathcal J (X, R) = \mathcal J_R X = (X \to R) \to X$. Given a selection function $\varepsilon : \mathcal J_S X$ and a lens $(v, u) : (X, S) \to (Y, R)$, we can "push forward" the selection function along the lens, to obtain a selection function $\mathcal J (v, u) (\varepsilon) : \mathcal J_R Y$ using the formula $\mathcal J (v, u) (\varepsilon) (k) = v (\varepsilon (\lambda x . u (x, k (v (x)))))$. This turns out to be functorial – pushing forward a selection function along the identity lens does nothing, and pushing forward a selection function along two lenses is the same as pushing forward along the composed lens.

The slick way to see this is to notice that there is a natural isomorphism $(X \to R) \to X \cong \mathbf{Lens} ((X, R), (1, 1)) \to \mathbf{Lens} ((1, 1), (X, R))$ – in other words, a selection function is a way to turn costates into states in the category of lenses. These two representable functors already play a [deep role](https://arxiv.org/abs/1711.07059) in open games.

I wanted to write a short blog post just to explain this much; how this connects to the monad structure of $\mathcal J_R$, and what are the implications for game theory, remain to be seen. To me, this functor feels close to the beating heart of game theory.
