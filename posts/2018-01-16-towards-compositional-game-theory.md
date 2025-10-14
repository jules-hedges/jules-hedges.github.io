---
title: Towards compositional game theory
date: 16/01/2018
---

- PhD thesis, Queen Mary University of London
- Links: [My preferred version](https://www.cs.ox.ac.uk/people/julian.hedges/papers/Thesis.pdf), [Official version](https://qmro.qmul.ac.uk/xmlui/handle/123456789/23259)

I wrote this not just as a thesis, but (against all advice) as a resource for *other people* to learn about open games. In spite of some problems, it will probably remain my preferred reference on open games for years to come. It contains plenty of its own introduction, so I won't introduce it again here.

There are two important pieces of **errata**:

- The SP-composition operator is *broken* and I no longer use it. I already knew this when I wrote the final version, so it's emphasised plenty of times already, but it's important enough that I mention it here again.
- There is a serious error in section 2.2.6, together with a portentous (and extremely embarrassing) footnote, where I assumed that all morphisms are comonoid homomorphisms. In fact that holds if and only if the underlying category is cartesian monoidal. This error was discovered by Josef Bolt. (A workaround is possible, but is work in progress and involves significantly more category theory.) On reflection, I wish I had specialised the entire thing to the category of sets.

I have resisted going down the rabbit-hole of keeping my thesis up-to-date, because correcting both of these would mean almost a complete rewrite with no benefit to me.

I learned the following things during and after writing my thesis:

- Writing a thesis takes [a lot longer](https://en.wikipedia.org/wiki/Hofstadter%27s_law) than it looks
- Investing two working days into learning about large-scale Latex programming was a good idea
- Naming Latex macros after their semantics rather than their syntax is a particularly good idea
- Although I still like the informal, rambling style I used, definitions, theorems and proofs should *definitely* be clearly demarcated
- I no longer want to write a book
