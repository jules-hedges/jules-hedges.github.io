---
title: A generalisation of Nash's theorem with higher-order functionals
date: 01/06/2017
---

- In *Proceedings of the Royal Society A*, 2013[^1]
- Links: [published article](https://royalsocietypublishing.org/doi/10.1098/rspa.2013.0041), [arXiv](https://arxiv.org/abs/1301.4845)
- DOI: [10.1098/rspa.2013.0041](https://royalsocietypublishing.org/doi/10.1098/rspa.2013.0041)

[^1]: Editor's note: This was meant to be the first of a series of posts about each of my published papers, but I only wrote one other, about my thesis.

This is my first paper, written in the first few months of my Ph.D. and published quickly. Four and a half years later it is still *technically* my best paper by the usual (wrong) metrics. Obviously now I wouldn't dare to do something as outrageous as submitting a paper to such a highly-ranked journal.

The paper defines a special case of [higher-order simultaneous games](https://www.cs.ox.ac.uk/people/julian.hedges/papers/games.pdf) for which mixed strategies can be defined, and to which the usual proof of Nash's existence theorem (by the [Kakutani fixpoint theorem](https://en.wikipedia.org/wiki/Kakutani_fixed-point_theorem)) still applies.

The main theorem of this paper makes strong assumptions, which have prevented it from being useful. My best guess now is that the proper generalisation of Nash's theorem to selection functions uses the [composition](https://arxiv.org/abs/1410.4353) of the selection monad with the finitary probability monad, with outcomes in a [convex space](https://arxiv.org/abs/0903.5522). (I don't know whether there is a fixpoint theorem for general convex spaces.[^2])

[^2]: Editor's note: A few years later I made a serious attack on this question together with Paolo Perrone and Sharwin Rezagholi, but we never reached a finished proof. I should write a blog post about it.

The only nontrivial example, the 'closed ball' selection function, makes another appearance in [Higher-order decision theory](https://www.cs.ox.ac.uk/people/julian.hedges/papers/decision.pdf), where it is related to [satisficing](https://en.wikipedia.org/wiki/Satisficing).

The only thing from this paper that I still like are the 'unilateral maps', which turned out to be useful for reasoning about higher order games. They appear again in [Selection equilibria of higher order games](https://www.cs.ox.ac.uk/people/julian.hedges/papers/games.pdf) and [Towards compositional game theory](https://www.cs.ox.ac.uk/people/julian.hedges/papers/Thesis.pdf), and are also closely related to continuations of open games.

Also, the copy editor for the *Royal* Society [made a mess of](https://en.wikipedia.org/wiki/American_and_British_English_spelling_differences) Her Majesty's language, including in the title.
