---
title: Towards dependent optics
date: 10/06/2020
---

There are two different generalises of [lenses](/posts/2018-08-16-lenses-for-philosophers.html) that are important in my research. One is [optics](https://arxiv.org/abs/1809.00738), which are a non-obvious generalisation of lenses that work over a monoidal category (whereas lenses only work over a finite product category). We use optics in [Bayesian open games](https://arxiv.org/abs/1910.03656), over the category of Markov kernels (kleisli category of probability). The other is dependent lenses, also known as containers and equivalent to polynomial functors. These haven't appeared in a game theory paper yet, but I use them privately to handle external choice of games better than lenses do.

An interesting and probably-hard question is to find a common generalisation of optics and dependent lenses.[^1] In this post I'll outline the problem and explain a (probable) partial solution that might be useful for somebody, but doesn't appear useful in game theory. This post will be heavy on category theory: I assume knowledge of fibred categories and the Grothendieck construction.

[^1]: Editor's note: In retrospect this was probably the greatest understatement of my career.

If $\mathcal C$ is a finite product category and $s$, $t$, $a$, $b$ are objects of $\mathcal C$, then a lens $(s, t) \to (a, b)$ is a pair of functions $v : s \to a$ and $u : s \times b \to t$. Lenses form a category $\mathbf{Lens} (\mathcal C)$. If $\mathcal C$ is only a monoidal category and we try to define a lens by $v : s \to a$ and $u : s \otimes b \to t$ then the composition fails to be associative, so they do not form a category.

There is one way to recover lenses over a monoidal category: we require $s$ and $a$ to be comonoids in $\mathcal C$, and we require $v : s \to a$ to be a comonoid homomorphism. For my particular use case this is no good. I take $\mathcal C$ to be the category of Markov kernels, in which every object is canonically a comonoid, but the comonoid homomorphisms are exactly the deterministic maps. In game theory the forwards part is *play* and is generally stochastic.

The "view" functor $\mathbf{Lens} (\mathcal C) \to \mathcal C$ given by $(s, t) \mapsto s$ and $(v, u) \mapsto v$ turns out to be a fibration. The fibre over $s$ turns out to be the opposite of the co-kleisli category of the $s \times -$ (reader) comonad on $\mathcal C$. (This co-kleisli category was characterised in Lambek and Scott's [Introduction to higher order categorical logic](https://github.com/Mzk-Levi/texts/blob/master/Lambek%20J.%2C%20Scott%20P.J.%20Introduction%20to%20Higher%20Order%20Categorical%20Logic.pdf): it is the "polynomial category" $\mathcal C [s]$ that results from freely adjoining a point $1 \to s$ to $\mathcal C$ and then closing under finite products.) So the category of lenses can be equivalently constructed via the Grothendieck construction:

$$ \displaystyle \mathbf{Lens} (\mathcal C) = \int^{s \in \mathcal C} \mathrm{coKl}(s \times -)^{\mathrm{op}} = \int^{s \in \mathcal C} \mathcal C [s]^{\mathrm{op}} $$

Next I'll talk about dependent lenses. I'm not sure exactly what structure you need on $\mathcal C$ to make it work, I think locally cartesian closed with pullbacks, but if in doubt I'm really thinking about $\mathcal C = \mathbf{Set}$. If $s$, $a$ are objects and $t(s)$ and $b(a)$ are dependent types (formally, "fibre bundles" $t \to s$ and $b \to a$), then a dependent lens $(s, t) \to (a, b)$ consists of $v : s \to a$ and (written in pseudo-Agda notation) $u : (s : S) \to b (v (s)) \to t (s)$, that is, $u : \prod_{s \in S} (b (v (s)) \to a (s))$. In purely categorical terms, this is a morphism $u : s \times_a b \to t$ such that the composite $s \times_a b \to t \to s$ equals the projection. They form a category, which I'll call $\mathbf{DLens} (\mathcal C)$.

David Spivak taught me this construction, which is apparently folkloric in algebraic geometry. It is a special case of morphisms of ringed spaces, where both spaces and rings are replaced by sets. Ordinary lenses are the full subcategory in which $t(s)$ and $b(a)$ are constants, or in fibrational terms, when $s \times t \to s$ and $a \times b \to a$ are the projections. $\mathbf{DLens} (\mathcal C)$ is also known as the [category of containers](https://people.cs.nott.ac.uk/psztxa/publ/fossacs03.pdf), and I believe that (maybe with some more assumptions) $\mathbf{DLens} (\mathcal C)$ is equivalent to the category of polynomial endofunctors and (all) natural transformations on $\mathcal C$.

I'll say something about why dependent lenses come up in game theory. $\mathbf{DLens} (\mathcal C)$ is complete and cocomplete, but in particular I need that it has all coproducts. $\mathbf{Lens} (\mathbf{set})$, on the other hand, only has coproducts of the form $(s, t) + (a, t) = (s + a, t)$. Coproducts of this form are used to construct the external choice operator on open games, which is extremely useful in practice: see sections 8-9 of [Morphisms of open games](https://arxiv.org/abs/1711.07059). If we try to take an external choice between open games with source types $(s, t)$ and $(a, b)$, then if we insert a state in $s$ then we get out a coutility in $t$, and if we insert a state in $a$ then we get out a coutility in $b$. Thus the type of coutility is the dependent type over $s + a$ that is $t$ over $s$ and $b$ over $a$.

In this sense dependent types enter the problem "naturally". Taking a coproduct of ordinary lenses, in general, will result in a dependent lens. Working with open games over the category of dependent lenses will make life easier by making the external choice operator defined everywhere, which makes the resulting graphical language (surface diagrams over a bimonoidal/rig category) significantly better behaved. The catch is that this can only be done for pure strategies, so my difficult question amounts to: how to do this for Bayesian open games? It can be *hacked* over lenses but is an unexploded bomb, because taking a cross section through a surface diagram need not result in a well-defined object.

There is still a fibration $\mathbf{DLens} (\mathcal C) \to \mathcal C$. This time it turns out that the fibre over $s$ is the opposite of the slice category $\mathcal C / s$. It turns out that $\mathcal C / s$ is also equivalent to the co-Eilenberg-Moore category of the $s \times -$ comonad. (I find this the most beautiful part of the whole story.) Thus we can also construct the category of dependent lenses by Grothendieck:

$$ \displaystyle \mathbf{DLens} (\mathcal C) = \int^{s \in \mathcal C} \mathrm{coEM} (s \times -)^{\mathrm{op}} = \int^{s \in \mathcal C} (\mathcal C / s)^{\mathrm{op}} $$

Finally on to optics. If $\mathcal C$ is a monoidal category and $s$, $t$, $a$, $b$ are objects then the appropriate generalisation of lenses, which are called optics $(s, t) \to (a, b)$, are elements of the coend (in the category of sets)

$$ \displaystyle \int^{x \in \mathcal C} \mathcal C (s, x \otimes a) \times \mathcal C (x \otimes b, t) $$

They form a category $\mathbf{Optic} (\mathcal C)$. When the monoidal product of $\mathcal C$ is a categorical product, then $\mathbf{Optic} (\mathcal C)$ is equivalent to $\mathbf{Lens} (\mathcal C)$, by an argument involving the [Ninja Yoneda Lemma](https://arxiv.org/abs/1501.02503) (which says that $\hom$ behaves like a Dirac measure for the integral). Optics have many interesting special cases and also can be generalised further to *mixed optics*, see [A categorical update](https://arxiv.org/abs/2001.07488).

One nice thing about the previous cases breaks down: there is no longer a view functor $\mathbf{Optic} (\mathcal C) \to \mathcal C$, let alone a fibration.

Here is one notion of "dependent optics" which might be useful for somebody, but is not what I need in game theory. This starts from [this MathOverflow post](https://mathoverflow.net/questions/205902/what-is-the-monoidal-equivalent-of-a-locally-cartesian-closed-category) (which was [brought to my attention by sarahzrf](https://x.com/sarah_zrf/status/1270394907291791360)) whose answers say that *in some sense* the appropriate analogue of $\mathcal C / s$ when $\mathcal C$ is monoidal is the category $\mathbf{Comod} (s)$ of [comodules over a commutative comonoid](https://ncatlab.org/nlab/show/comodule) $s$ in $\mathcal C$.

A comonoid homomorphism $s \to a$ appears to lift to a functor $\mathbf{Comod} (s) \to \mathbf{Comod} (a)$ (for the slice category this is contravariant, this is one point that I don't understand yet). A morphism which is not a comonoid homomorphism [does not lift in this way](https://x.com/_julesh_/status/1270677270529744896). So I believe we can define a category of sort-of "dependent optics" by Grothendieck construction:

$$ \displaystyle \mathbf{DOptic} (\mathcal C) = \int^{s \in \mathbf{Comon} (\mathcal C)} \mathbf{Comod} (s)^{\mathrm{op}} $$

Unfortunately this means that the forwards maps in the category category are forced to be comonoid homomorphisms, so this is actually a generalisation of *lenses* over a monoidal category as I defined at the beginning, rather than a generalisation of optics, and are not suitable for my problem for the same reason.

Fundamentally I expect the "true" category of dependent optics will not be a fibred category, as ordinary optics are not, and so this strategy of defining the category by Grothendieck construction starting from a generalisation of the individual fibres was doomed from the start.
