---
title: "Folklore: Monoidal kleisli categories"
date: 18/04/2019
---

I've been threatening a few times recently to blog about bits of mathematical folklore that I use, i.e. important things that aren't easy to find in the literature. I'm going to start with an easy one that won't take me long to write.

**Theorem**: Given a commutative strong monad on a symmetric monoidal category, the kleisli category is symmetric monoidal in a canonical way.

Defining the structure is easy: the monoidal product of objects is the underlying monoidal product, and the monoidal product of morphisms essentially *is* the strength map. Commutativity is used in proving that the monoidal product is a bifunctor.

The only place I've seen this stated and proved is corollary 4.3 Power and Robinson's paper [Premonoidal categories and notions of computation](http://www.eecs.qmul.ac.uk/~edmundr/pubs/mscs97/premoncat.ps). I feel like it should be much older — Anders Kock certainly could have discovered it — but I haven't found it anywhere else. (*See edit below*)

I use this theorem quite regularly for specific examples, especially on the (cartesian monoidal) category of sets. The powerset monad induces the category of relations with the monoidal product given by underlying cartesian product. The finite support distribution monad gives the category that goes by various names such as stochastic relations, stochastic matrices, stochastic processes, conditional distributions. The reader monad gives a thing called a 'polynomial category', a major topic in Lambek and Scott's book Introduction to Higher Order Categorical Logic.

The important condition is that the monad is commutative, which is a property of the strength. My favourite way to define commutativity is that the Haskell snippets `do {x <- a; y <- b; return (x, y)}` and `do {y <- b; x <- a; return (x, y)}` are interchangeable. In other words, the computational effect defined by the monad does not introduce any implicit dataflow.

If the monad is not commutative then the structure defined on the kleisli category is symmetric *pre*monoidal, which is the weakening of a monoidal category where the monoidal product is only required to be individually functorial in its two places, but not a bifunctor.

As a final comment, Jeffrey's paper [Premonoidal categories and a graphical view of programs](https://asaj.org/papers/premonA.pdf) (as well as being ahead of its time by using html as a distribution method) is about the equivalent of string diagrams in premonoidal categories. I'd like to come back to this paper one day using the machinery we have nowadays.

**Edit** (4 months later): Recently I came across an earlier reference for this result: It is corollary 7 in [Tenseurs et machines](https://www.numdam.org/article/CTGDC_1980__21_1_5_0.pdf) by René Guitart, published in 1980. I wish I could read French, because this paper looks extremely ahead of its time. (I learned about it from [A synthetic approach to Markov kernels...](https://arxiv.org/abs/1908.07021) by Tobias Fritz.)
