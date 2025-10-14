---
title: Open games with stateful payoffs
date: 17/05/2020
---

I'm starting to worry that [my open games implementation](https://github.com/CyberCat-Institute/open-game-engine) is getting ahead of what I've written in papers in a few ways, and I should correct that with documentation blog posts. This one is about the module [`OpenGames.Engine.StatefulPayoffs`](https://github.com/CyberCat-Institute/open-game-engine/blob/og-v0.1/src/OpenGames/Engine/StatefulPayoffs.hs), which is a pragmatic solution to a fundamental conceptual problem with open games: the identity of agents is not well-defined. Rather the fundamental unit is a decision, and if two decisions are made by the same agent then it is the user's responsibility to make sure that those decisions have the same payoff, or at least game-theoretically "coherent" payoffs.

For a long time I thought this was a conceptual problem but not a practical one. But recent work with my collaborators Philipp Zahn, Seth Frey and Joshua Tan has stress-tested open games in new ways and revealed it to a problem after all.[^1] Specifically, if one agent makes 2 decisions on different sides of an abstraction boundary, then the programmer must explicitly design the boundary to accommodate that agent's payoff. This feels like an abstraction leak.

[^1]: Editor's note: This is the work that eventually led to [this paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0283361).

The solution I came up with is to put the backwards part of an open game into a state monad, so that different decisions made by the same agent implicitly coordinate through a global stable variable. Loosely speaking, this means augmenting an open game with an imperative program that runs backwards in time and transforms payoffs. I find it neat that the state monad is normally used to describe something non-compositional in programming, namely mutable global state, and it can also be used to describe something non-compositional in game theory, namely that an agents' identities cut across the structure of a game.

The main technical component is *monadic lenses*, due to [this paper](https://arxiv.org/abs/1601.02484). (They are the simplest example of a [mixed optic](https://arxiv.org/abs/2001.07488).) A monadic lens $(S, T) \to (A, B)$ for the monad $M$ consists of a view function $S \to A$ and an update function $S \times B \to M T$, thus the update (but not the view) lives in the kleisli category. (Putting view in the kleisli category is much harder, and requires passing from lenses to optics, as we do in Bayesian open games.) Monadic lenses compose in an entirely obvious way to form a category, which is a monoidal category if $M$ is commutative (see [monoidal kleisli categories](/posts/2019-04-18-folklore-monoidal-kleisli-categories.html)).

Of course we take $M$ to be a state monad, which is not commutative, which means that lenses only form a [premonoidal category](https://ncatlab.org/nlab/show/premonoidal+category). This isn't a problem in practice because my implementation uses a sequential DSL that can't directly express simultaneous play, just like do-notation.

We take the type of the state variable to be the type of payoff vectors, $\mathcal{A} \to \mathbb{R}$ where $\mathcal{A}$ is the type of agents. In practice, so far I've taken the type of agents to be `String`, so an agent's identity is its name and I don't have to worry about how to modify $\mathcal{A}$ when embedding a game into a context with more agents. (It should probably be a `Data.Map`, but performance isn't important here so I just used a function.)

The appropriate type of contexts for an open game $(S, T) \to (A, B)$ when using monadic lenses turns out to be the obvious thing, namely $S \times (A \to M B)$. With this, open games can be defined and they form a premonoidal category.

The interesting part is how to define decisions, which now involves a lot of moving parts. A decision is an open game of type $(X, 1) \to (Y, \mathbb{R})$, and we fix an agent $a \in \mathcal{A}$ making the decision. The strategies are still $X \to Y$ as usual. For a fixed strategy $\sigma : X \to Y$, the forwards part of the resulting lens is just $\sigma$.

The backwards part is a function $X \times \mathbb{R} \to M 1$. Usually for a decision this coplay function is trivial, but here we instead do something creative: we take the agent $a$‘s existing payoff from the state variable, and we numerically *add* the payoff of the decision. There are two aspects of this: Why modify the payoff, and why add it in particular?

The second thing is easy: I had to do something, and guessed that addition was probably going to be the most useful in practice. The first thing is subtle. Because the imperative code is running backwards in time (contravariantly), the decision is modifying the payoff of past decisions by the same agent. This works because payoffs in the past are like sunk costs: an agent is indifferent to anything you do to its payoff in the past (and dually, an imperative program's behaviour is independent of changes made to its state in the future). I don't think it's obvious that this hack works, but it does seem to work.

The most complicated part is the equilibrium check. We are given a context $(h, k)$ where $h \in X$ is the history and $k : Y \to M \mathbb{R}$ is the continuation. We convert $k$ into an ordinary function $k' : Y \to \mathbb{R}$ by escaping the state monad: we initialise the state with the identically zero payoff vector, and then at the end we add the final value of agent $a$‘s state variable to the return value to produce the total payoff (throwing away the payoffs of the other agents).

From here$ it's business as usual, we check whether $\sigma (h)$ maximises $k'$. In practice this means querying $k'$ at every counterfactual value $y$, and each of these means rerunning the imperative program with a different input and a re-initialised state. (Thus it acts like a global state to coplay, but only a local state to the equilibrium checker.)

We can do some other tricks too. Obviously we can nonlocally modify an agent's payoff without any decision being made. (One application I have in mind for this is discounting.) My favourite of these is decisions which are not made by a fixed agent, but by one decided endogenously, something that would otherwise be a pain to express (using external choice, probably the subject of a future blog post).

Specifically, we can make a variant decision (which we call a 'role decision') of type $(\mathcal{A} \times X, 1) \to (Y, \mathbb{R})$ whose set of strategies is still $X \to Y$ (although I have an idea to extend this to $\mathcal{A} \times X \to Y$, allowing each agent to have a different contingent strategy for if they are chosen to make the decision). The equilibrium checker now takes a state $(a, h, k)$, and this variable $a$ is used to index into the state vector.

The main thing on the todo list is to make this work for Bayesian games, since it currently only works for pure strategies (in turn, because monadic lenses are easier to understand). I expect this to start from mixed optics where the forwards part lives in the kleisli category of probability, and the backwards part lives in the kleisli category of the monad transformer stack consisting of probability below and state above.[^2]

[^2]: Editor's note: The ["stateful Bayesian" backend](https://github.com/CyberCat-Institute/open-game-engine/blob/master/src/OpenGames/Engine/BayesianGames.hs) eventually became the default backend of the open game engine.
