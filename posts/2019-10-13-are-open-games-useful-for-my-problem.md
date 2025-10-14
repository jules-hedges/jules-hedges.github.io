---
title: Are open games useful for my problem?
date: 13/10/2019
---

Recently open games have reached a state where they realistically might start becoming useful to some people (see [this demo](https://www.youtube.com/watch?v=4Ln4FukIFO0)). I want to write down the current state of the art, what I believe is possible and what I believe are the limitations of compositional methods. This guides which sorts of situations I believe open games will be applicable to.

First I will say what open games can already do (at least in theory). Then I will say what is awkward and what I think is probably possible with future work, and finally when I think you shouldn't use open games. This post is written for a hypothetical reader who wants to use game theory as a modelling paradigm.

# What open games can already do

- Open games are compositional – that's the point. If you are studying different situations that contain some "common sub-systems" (for example institutions) then open games might be useful to you.
- Open games support a graphical notation (string diagrams) that's very intuitive. It's exponentially more compact than extensive form trees, it displays the structure and information flow of the game, but omits most of the details. I expect end-users to mainly work at this level, with software handling everything under the hood.
- Real numbers and maximisation are not essential. Players in open games can have goals defined by any [selection function](https://www.cs.ox.ac.uk/people/julian.hedges/papers/decision.pdf), a big design space that is still mostly unexplored. Satisficing, for example, can be expressed easily.
- (Weak) Bayesian Nash equilibria of open games are understood — see [this paper](https://arxiv.org/abs/1910.03656). This is quite an expressive class of games that can exhibit some subtle behaviours such as signalling. The theory is involved, but it can be very neatly encapsulated in Haskell (or any language with type quantification) and hidden from end users.
- Repeated games can be handled neatly — see [this paper](https://arxiv.org/abs/1711.07968). (I believe that it is better to handle discounting of payoffs stage-wise using [metric coinduction](https://arxiv.org/abs/0908.2793), which is only a small modification to their construction.)
- Related to the previous point, based on unpublished notes I believe that open games are deeply 'compatible' with dynamic programming and the Bellman equation.

# What is awkward

- Open games work the most cleanly in situations where the 'protocol' (order of play, sets of choices, etc.) do not depend on previous choices (equivalently, when the extensive-form tree would be perfectly balanced). When this is not the case the diagrams become more complicated (a collection of 2-dimensional diagrams glued together in a 3-dimensional way) and the theory becomes more difficult.
- Open games say nothing about the (infamous) problems of equilibrium-based modelling. I expect that standard 'fixes' such as bounded rationality and universal type spaces can be incorporated, but that is future work.
- Explicit representations of beliefs and belief updating (as in strong Bayesian Nash equilibrium) can probably be incorporated, but it is future work.
- I believe that games on networks can be handled nicely, by combining with category-theoretic approaches to network theory (for example [decorated cospans](https://arxiv.org/abs/1502.00872)), but this is future work.
- Cooperative/coalitional game theory might be possible or not — I have no idea yet.

# When not to use open games

- I expect open games to work best at intermediate-scale (though I'm not certain what that means in practice). If your problem is small, traditional game theory is probably better. If your problem is very large but has symmetries you can exploit (for example a large population of agents with varying parameters) traditional game theory might be better too. (For example, many people have asked me about consensus in blockchains, which I believe is an example of the second case.)
- Open games are equilibrium-based. If you only care about simulation then agent-based modelling techniques are far more flexible and also compositional.
- Open games say nothing about the problem of algorithmically computing equilibria. If I am optimistic, they might have a role to play in cases where one wants to find solutions compatible with some data about sub-systems. On the plus side, it should be possible to make open games software interoperable with [Gambit](http://www.gambit-project.org) or other tools.
