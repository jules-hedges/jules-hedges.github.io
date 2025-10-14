---
title: Subgame perfection made difficult
date: 26/05/2020
---

This is the second post in catching up on aspects of [open-games-hs](https://github.com/CyberCat-Institute/open-game-engine) that are ahead of my papers, following [open games with stateful payoffs](https://julesh.com/2020/05/17/open-games-with-stateful-payoffs/). Subgame perfection has been an embarrassing thorn in my side since 2016 when I had to do major surgery on [my PhD thesis](https://julesh.com/2018/01/16/towards-compositional-game-theory/) because the category of "open games with subgame perfect equilibria" turned out to not be monoidal. Currently there are two approaches: One in [iterated open games](https://arxiv.org/abs/1711.07968) which is quite pragmatic and requires the "user" specifying an open game to manually mark where the subgames are by applying a functor; and one in [morphisms of open games](https://arxiv.org/abs/1711.07059) which I find very elegant but requires both an extra categorical dimension and an equivalent amount of effort by the "user".

I always wanted an "automatic" approach to subgame perfection in open games, like I failed to do in my thesis – just draw the usual string diagram, and get subgame perfect equilibria out. I now have a way to do it, implemented in [`OpenGames.Engine.SubgamePerfect`](https://github.com/CyberCat-Institute/open-game-engine/blob/og-v0.1/src/OpenGames/Engine/SubgamePerfect.hs), which I'll document here.

The starting point is an idea that doesn't work. In the category of deterministic open games, a decision $\mathcal{D} : (X, 1) \to (Y, \mathbb{R})$ has strategy profiles $\Sigma_\mathcal{D} = X \to Y$, and equilibria $\mathbf{E}_\mathcal{D} : X \times (Y \to \mathbb{R}) \to \mathcal{P} (\Sigma_\mathcal{D})$ given by $\mathbf{E}_\mathcal{D} (h, k) = \{ \sigma \mid \sigma(h) \in \arg\max (k) \}$. Here $h$ plays the role of the on-equilibrium subgame. Here is an idea that doesn't work: instead define it by $\mathbf{E}_\mathcal{D} (h, k) = \{ \sigma \mid \sigma(x) \in \arg\max (k) \text{ for all } x \in X \}$.

To see why this doesn't work, form the game $\mathcal{D}^\Delta = (1_X \otimes \mathcal{D}) \circ \Delta : (X, 1) \to (X \times Y, \mathbb{R})$, where $\Delta : (X, 1) \to (X \times X, 1)$ lifts the copy function. Given a context with history $h : X$ and continuation $k : X \times Y \to \mathbb{R}$, its equilibrium condition $\mathbf{E}_{\mathcal{D}^\Delta} (h, k)$ is equivalent to $\mathbf{E}_{1_X \otimes \mathcal{D}} ((h, h), k)$ and then to $\mathbf{E}_{\mathcal{D}} (h, k')$ where $k' (y) = k (h, y)$. If we keep the true definition of $\mathbf{E}_{\mathcal{D}}$ then this is carefully balanced to give us Nash. But if we use our proposed modified definition, the equilibrium condition becomes $\sigma (x) \in \arg\max_{y \in Y} k (h, y)$ for all $x \in X$. This is wrong – the subgame perfect equilibrium condition should be $\sigma (x) \in \arg\max_{y \in Y} k (x, y)$ for all $x \in X$.

What is happening is that the context of $\mathcal{D}$ should keep track of how a conterfactual change to its history, from the on-equilibrium subgame $h$ to an off-equilibrium subgame $x$, should propagate to the continuation, which should change from $k (h, -)$ to $k (x, -)$.

Fortunately we're in luck. The heavy machinery developed in [Bayesian open games](https://arxiv.org/abs/1910.03656) is able to handle exactly this sort of situation. The first thought is to replace the category of stochastic maps with the category of relations. Pushing forward the state $X \subseteq X$ through the copy map $\Delta$ yields the diagonal set $\Delta^* (X) = \{ (x, x) | x \in X \} \subseteq X \times X$, which contains exactly enough information to propagate a counterfactual change in one output backwards and then forwards again to the other output. That is to say, if we take the pushfoward state $\Delta^* (X)$ and condition on one output being some $x$, the other output collapses to the same $x$. This much is standard reasoning in the category of relations.

This is almost right, but not quite. We also need to keep track of what *actually* happens, and not just the counterfactual alternatives, in particular because they are still used for payoffs: a player's preferences are indifferent to payoffs that might have occurred, but did not. We actually need to simultaneously keep track of 3 levels of possibility: (1) what *actually* happened, (2) what counterfactually *might have* happened, but did not, and (3) what *could not* have happened. Pushing forward through $\Delta$ illustrates this well. If the input play is $h$ then what actually happens is $(h, h)$, what could have happened is any element of $\Delta^* (X) = \{ (x, x) \mid x \in X \}$, and any $(x, x')$ with $x \neq x'$ could not have happened even counterfactually.

This motivates the following definition. Let $T : \mathbf{Set} \to \mathbf{Set}$ be given by $T (X) = \{ (x, S) \mid x \in S \subseteq X \}$. I call T the *pointed powerset monad*, and it is a hybrid of the identity and powerset monads. The monad unit is given by $\eta (x) = (x, \{ x \})$, and the multiplication by $\mu ((x, S'), S) = (x, \bigcup S)$. $T$ admits monad morphisms to both identity and powerset. The kleisli category of $T$ is "pointed relations": a kleisli morphism $X \to Y$ is a relation $R \subseteq X \times Y$ together with a function $f : X \to Y$ satisfying $(x, f (x)) \in R$ for all $x$. Since $T$ is commutative, its kleisli category [admits a monoidal product](https://julesh.com/2019/04/18/folklore-monoidal-kleisli-categories/). (I've never seen this monad before in the literature.)

Fortunately we can save some work and *hack* this monad in Haskell by pretending that ordinary lists are pointed subsets, using the head as the basepoint. Nothing goes wrong as long as we are careful to only use lists in ways that are invariant under permutation and duplication (ie. we treat free monoids in ways that are also valid for free commutative idempotent monoids). All of the list monad operations are "head-preserving" in the appropriate way.

Just as for Bayesian open games, we set up [coend optics](https://arxiv.org/abs/1809.00738) for the kleisli category of this monad. In category theoretic notation a kleisli optic $(X, S) \to (Y, R)$ is an element of $\int^A (X \to T (A \times Y)) \times (A \times R \to T S)$, and in Haskell notation it is an element of the GADT
```haskell
data Optic x s y r where
  Optic :: (x -> [(a, y)]) -> (a -> r -> [s]) -> Optic x s y r
```
The type of contexts is also exactly as for Bayesian open games: it is an element of $\int^A T (A \times X) \times (A \times Y \to T R)$ or
```haskell
data Context x s y r where
  Context :: [(a, x)] -> (a -> y -> [r]) -> Context x s y r
```
The definition of the monoidal category of open games can be done generically for an arbitrary monad, so almost all of the code follows for free.

The *only* thing that needs to be specialised to the pointed powerset monad is the definition of a decision $\mathcal{D} : (X, 1) \to (Y, \mathbb{R})$. This isn't trivial. The set of strategies is still $\Sigma_{\mathcal{D}} = X \to Y$. For a strategy $\sigma : X \to Y$ we must produce an optic $(X, 1) \to (Y, \mathbb{R})$. We take the bound variable to be $A = 1$, so the backward part is trivial and we need only give the view $v : X \to T Y$. We take $v (x) = (\sigma (x), Y)$, the pointed subset $Y \subseteq Y$ with basepoint $\sigma (x)$. This means that what happens is $\sigma (x)$, but any element of $Y$ could happen counterfactually, because the agent has a free choice. (This is an unusually clear appearance of *free will* in mathematics.)

In Haskell I always write decisions to take an exhaustive list of possible moves as a parameter in order to brute force $\arg\max$, because implementing a decent $\arg\max$ is an orthogonal problem. So the implementation of a decision begins:
```haskell
decision :: [y] -> OG (x -> y) x () y Double
decision ys = OG {
  play = \f -> let v x = zip (repeat ()) (f x : ys)
                   u () _ = [()]
                in Optic v u
...
```

Here `f x : ys` is the list representing the pointed subset $(f (x), Y)$.

Finally, we must evaluate a strategy $\sigma : X \to Y$ in a context, which is a triple $(A, h, k)$ with $h : T (A \times X)$ and $k : A \times Y \to T \mathbb{R}$. Here we get right to the heart of what subgame perfection is all about: we ignore the factual part of $h$ and only use the set of counterfactuals, but we ignore the counterfactual part of $k$ and only use the factual. Specifically, the equilibrium condition is that for every $(a, x) \in h$, we have $\sigma (x) \in \arg\max_{y \in Y} k (a, y)_*$, where $k (a, y)_*$ means the basepoint of $k (a, y) : T \mathbb{R}$. Information about $x$ is transferred to $k$ via $a$, which is *entangled* with $x$ in the state $h$.

In Haskell, the definition concludes:
```haskell
...
  equilibrium = \(Context h k) f ->
    and [head (k a (f x)) >= head (k a y) | (a, x) <- h, y <- ys]}
```

And that's it! I have no intention to *prove* that this always gives subgame perfect equilibrium any time soon – correctness proofs for open games are always extremely painful (they go by induction on extensive form) and I put them off as long as I realistically can. It passes the [ultimatum game](https://en.wikipedia.org/wiki/Ultimatum_game) test, which is illustrated in [OpenGames.Examples.SubgameTest](https://github.com/CyberCat-Institute/open-game-engine/blob/og-v0.1/src/OpenGames/Examples/SubgameTest.hs). For me this is enough evidence to start using it first and ask questions later.
