---
title: The cursed families fibration
date: 16/08/2023
---

This post is describing a simple construction I worked out while trying to understand the [semantics of quantitative type theory](https://bentnib.org/quantitative-type-theory.html). It is a variant of the families fibration, a well known construction in category theory, which is sensitive to the way that sets are made out of elements, and allows fibres to overlap. Hence it is "cursed", which is like [evil](https://ncatlab.org/nlab/show/principle+of+equivalence) but more.

Let's begin by recalling how the families fibration works. For any category $\mathcal C$, we can build a category $\mathbf{Fam} (\mathcal C)$. The objects of $\mathbf{Fam} (\mathcal C)$ are set-indexed families in $\mathcal C$: that is, a pair of an index set $X$ and a function $A : X \to \mathrm{Ob} (\mathcal C)$. A morphism $(X, A) \to (Y, B)$ is a pair of a function $f : X \to Y$ and an $X$-indexed family of morphisms of $\mathcal C, A (x) \to B (f (x))$.

There is a functor $\mathbf{Fam} (\mathcal C) \to \mathbf{Set}$ which forgets everything except the index set. This functor turns out to be a fibration. The fibre over a set $X$ is exactly $\mathcal C^X$, ie. the functor category $X \to \mathcal C$ when $X$ is seen as a discrete category.

Now let's specialise to $\mathcal C = \mathbf{Set}$, so we are considering the fibration $\mathbf{Fam} (\mathbf{Set}) \to \mathbf{Set}$. A magical property of the category of sets is that $\mathbf{Set}^X \cong \mathbf{Set} / X$. Let's go through how this equivalence works. In the forwards direction, we have an $X$-indexed family $A : X \to \mathrm{Ob} (\mathbf{Set})$, which we turn into its disjoint union equipped with the projection onto the index set, $\sum_{x \in X} A (x) \to X$. In the backwards direction, we have a function $f : X' \to X$, which we turn into an $X$-indexed family by inverse images: $A (x) = \{ x' \in X' \mid f(x') = x \}$. Both of these turn out to be functorial and define an equivalence of categories.

Doing this on the total category gives us an equivalence of categories $\mathbf{Fam} (\mathbf{Set}) \cong \mathbf{Set}^\to$, where $\mathbf{Set}^\to$ is the arrow category, whose objects are functions and morphisms are commuting squares. The equivalence looks basically the same: an object $(X, A)$ of $\mathbf{Fam} (\mathbf{Set})$ goes to the object $\sum_{x \in X} A(x) \to X$ of $\mathbf{Set}^\to$, and the object $f : X' \to X$ of $\mathbf{Set}^\to$ goes to the object $(X, f^{-1} (x)_{x \in X})$ of $\mathbf{Fam} (\mathbf{Set})$.

This extends to an equivalence of fibrations, where the families fibration corresponds to the codomain fibration $\mathrm{cod} : \mathbf{Set}^\to \to \mathbf{Set}$.

Ok, that concludes our very condensed recap of the families construction. Now let's make it *cursed*.

The objects of $\mathfrak{Fam}$ (fraktur is the appropriate font for a cursed object) are the same as the objects of $\mathbf{Fam} (\mathbf{Set})$: a pair of a set $X$ and a function $A : X \to \mathrm{Ob} (\mathbf{Set})$. However, we restrict the morphisms (so we get a wide subcategory). A morphism $(X, A) \to (Y, B)$ is a pair of a function $f : X \to Y$ and an $X$-indexed family functions $f' (x, -) : A (x) \to B (f (x))$ satsifying an additional property: that if $a \in A (x_1) \cap A (x_2)$, then $f' (x_1, a) = f' (x_2, a)$, and consequently, both are elements of $B (f (x_1)) \cap B (f (x_2))$. (And if this intersection is empty then no morphism of this type can exist.)

This is fundamentally using the fact that $\mathbf{Set}$ is untyped, and we can talk about the intersection of arbitrary sets. 

The intuition is that morphisms are not allowed to use their knowledge of the index, unless it can be deduced from the rest of their input. I'll go through a detailed example of this later.

Just as there is an equivalence of categories $\mathbf{Fam} (\mathbf{Set}) \cong \mathbf{Set}^\to$, there is a cursed counterpart of $\mathbf{Set}^\to$ that is equivalent to $\mathfrak{Fam}$. I'll call this category$ \mathbf{Set}^\leftrightarrow$. An object of $\mathbf{Set}^\leftrightarrow$ is a pair of sets equipped with a binary relation $R \subseteq X \times X'$. A morphism from $R \subseteq X \times X'$ to $S \subseteq Y \times Y'$ is a pair of functions $f : X \to Y$ and $f' : X' \to Y'$ such that if $x R x'$ then $f (x) S f' (x')$.

One perspective on $\mathbf{Set}^\leftrightarrow$ is that it's the category of morphisms in the double category $\mathbf{Rel}$ whose proarrows are binary relations.

We can turn an object $(X, A)$ of $\mathfrak{Fam}$ into an object of $\mathbf{Set}^\leftrightarrow$ by taking the non-disjoint union $\cup_{x \in X} A (x)$ together with the relation $R \subseteq X \times \cup_{x \in X} A (x)$ given by $x R x'$ if $x' \in A (x)$. Conversely, a binary relation $R \subseteq X \times X'$ we can do the relational equivalent of inverse images to get the $X$-indexed set $A (x) = \{ x' \in X' \mid x R x' \}$.

Both $\mathfrak{Fam}$ and $\mathbf{Set}^\leftrightarrow$ are fibred over $\mathbf{Set}$, the latter being by the relational version of codomain, and the equivalence of categories $\mathfrak{Fam} \cong \mathbf{Set}^\leftrightarrow$ extends to an equivalence of fibrations.

Here is a simple example, which was suggested by Zanzi. Given a set $X$, we have the set $X^*$ of lists, the set $X^n$ of lists of length $n$, and the set $X^{* \leq n}$ of lists of length at most $n$. The sets $X^n$ are non-overlapping for distinct $n$, that is to say, two lists of different lengths cannot be equal. $X^*$ is in natural bijection with the disjoint union $\sum_{n \in \mathbb N} X^n$ (whose elements are pairs of a number and a list of that length), and is equal on the nose to the non-disjoint union $\cup_{n \in \mathbb N} X^n$. 

On the other hand, the sets $X^{* \leq n}$ overlap: $X^{* \leq m} \subseteq X^{* \leq n}$ iff $m \leq n$. The disjoint union $\sum_{n \in \mathbb N} X^{* \leq n}$ consists of pairs of a list and an upper bound on its length. But the non-disjoint union $\cup_{n \in \mathbb N} X^{* \leq n}$ is still equal on the nose to $X^*$.

Suppose we want to talk about the length function, in $\mathfrak{Fam}$ considered as a baby model of quantitative type theory. Consider the object $(\mathbb N, X^{* \leq n}_{n \in \mathbb N})$, which models an "irrelevant Sigma type" where an upper bound on the length is available at the type level but may not be used at the value level.

The set $\mathbb N$ is not a dependent type, so we consider it trivially $1$-indexed, by the object $(\{ * \}, \mathbb N_{* \in \{ * \}})$. There is a morphism $(\mathbb N, X^{* \leq n}_{n \in \mathbb N}) \to (\{ * \}, \mathbb N_{* \in \{ * \}})$, given by the unique function $\mathbb N \to \{ * \}$ together with the $\mathbb N$-indexed family of functions $f (n, -) : X^{* \leq n} \to \mathbb N$ that ignores $n$ and returns the actual length of the list. This satisfies the condition to be a morphism of $\mathfrak{Fam}$.

We can say the same thing equivalently in $\mathbf{Set}^\leftrightarrow$, where the corresponding object is the relation $R \subseteq \mathbb N \times X^*$ given by $n R x$ if the length of $x$ is at most $n$. The object corresponding to $\mathbb N$ is the relation $S \subseteq \{ * \} \times \mathbb N$ given by $* S n$ for all $n$. The corresponding morphism is given by the unique function $\mathbb N \to \{ * \}$ and the length function $X^* \to \mathbb N$. This satisfies the condition to be a morphism of $\mathbf{Set}^\leftrightarrow$.

That's all for today, but I'll leave a puzzle. All of this was very heavily specialised to working concretely with sets. What structure could we put on a category in order that we can talk about "intersections of objects" in this way? [Bruno](https://www.brunogavranovic.com/) suggested that what we need is a Grothendieck (possibly pre-)topology. I find this very plausible: two objects have nonempty overlap if they admit covering families with an object in common. But I'll definitely leave the details of this for another day.
