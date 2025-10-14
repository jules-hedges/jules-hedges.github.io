---
title: Breaking the rules
date: 16/08/2017
---

As might be expected, the *rules of the game* are an important concept in game theory. But the way that game theory treats its all-important rules is very un-subtle: it is firmly built into the epistemic foundations that the rules are common knowledge, which makes it extremely difficult to talk about *breaking the rules*. If any player breaks the rules, or even if any player *suspects* another player of breaking the rules (up to any level of epistemic reasoning), you are simply outside the scope of your model. Of course the possibility of breaking any individual rule, and the consequences for doing so, can be manually built into your game, but then it is unclear whether it can reasonably be called a 'rule' any more.

This is discussed by David Kreps in chapter 5 of his book [Game Theory and Economic Modelling](https://ekonomika.be/wp-content/uploads/2020/03/David_M_Kreps_Game_theory_and_economic_model.pdf/download/), where he discusses the (many and serious) deficiencies of game theory:

> "If game theory is necessarily so limited, then it is extremely limited as a tool of  analysis for *some* important economic problems. And I would prefer that we not give up hope, but instead think in terms of how we might adapt the theory so it is better able to cope with ambiguous protocols."

(Incidentally, I like Kreps' term 'protocol' to refer to the rules, which better reflects their status and distinguishes them from the natural meaning of 'rules', which can indeed be broken.)

Here are some examples of rule-breaking, all of which pose a serious challenge to the modelling power of game theory. Some of them come from things that we naturally call 'games', and some are from situations that we would not naturally call games, but which we would like to model using game theory.

- I am playing chess, and move my piece in an illegal move across the board

In this case the position of game theory is defensible on grounds of common sense: if I do this it is no longer reasonable to claim that I am 'playing chess' at all.

- [Gaioz Nigalidze](https://en.wikipedia.org/wiki/Gaioz_Nigalidze) is playing chess and uses a smartphone running a chess engine hidden in a bathroom[^1]

[^1]: Editor's note: At the time of writing that was the most recent high-profile cheating scandal in chess, but the later [Carlsen-Niemann controversy](https://en.wikipedia.org/wiki/Carlsen%E2%80%93Niemann_controversy) is much, much funnier.

The difference between the previous two examples is just the difference between the *rules of chess* and the *rules of the Dubai Open*. However my brain naturally screams that this is an issue of *extensionality vs. intensionality*: the former is an *illegal move*, which is an extensional (or positivist) concept, whereas  the latter is an *illegal strategy* (or method of computing a move), which is an intensional concept.

- I am playing chess against my little sister. After a while she gets bored and frustrated, grabs my king and hides it behind her back

Are we still doing combinatorial game theory? Anyway, on to football (soccer), that [well-known bastion](https://en.wikipedia.org/wiki/Premier_League) of the sanctity of the rules.

- A player in football makes a dangerous tackle, and the referee awards a free kick to the other side

Football is a contact sport, and this happens many times in the course of every game. In what sense is it 'against the rules' to make a dangerous tackle?

- A player makes a tackle with [her](https://en.wikipedia.org/wiki/Women%27s_association_football) boot raised high. She doesn't make contact with the other player, but the referee gives her a yellow card anyway

There are two views of this. One is that tackles of this kind are against the rules irrespective of whether it makes contact (or to say it another way, if this happens then the *correct* action of the referee, by definition, is to award a yellow card). Another view is that the referee is reasoning counterfactually: *if* the tackle had made contact *then* it would have been dangerous.

- A player [dives](https://en.wikipedia.org/wiki/Diving_(association_football)), and successfully tricks the referee into awarding a foul against the other team

This is an example of meta-level reasoning about the rules, exploiting the known [bounded rationality](https://en.wikipedia.org/wiki/Bounded_rationality) of the referee's ability to enforce the rules correctly.

- A player in football takes one for the team, committing an obvious foul in expectation of being caught, in order to prevent an open goal

This example gets even more interesting if either (a) the player in question is given a monetary fine for the foul, or (b) they do not commit a foul but are then criticised by their team, fans or pundits for not doing so.

[American football](https://en.wikipedia.org/wiki/American_football) gives us this nice puzzle:

- A team commits a delay of game penalty in order to give their kicker more room

For those not in the know, this is a fairly common situation where the 'penalty' for breaking the rule is actually to a team's advantage. This is a 'technical foul' that is universally considered to not be a foul at all. This is probably the most borderline case I know of whether something is against the rules or not.

Enough of sports, onto economics and politics.

- [Two](https://en.wikipedia.org/wiki/Tesco) [companies](https://en.wikipedia.org/wiki/Sainsbury%27s) [collude](https://en.wikipedia.org/wiki/Collusion) on prices, and [are not caught](https://en.wikipedia.org/wiki/Defamation)
- A company spends money to [lobby](https://en.wikipedia.org/wiki/Lobbying) a government to [relax](https://en.wikipedia.org/wiki/Tobacco_politics) or [increase](https://en.wikipedia.org/wiki/Energy_subsidy) regulation, to increase its competitiveness in a market
- The law is (either quickly or slowly) changed to regulate a newly-invented [weapon](https://en.wikipedia.org/wiki/Overview_of_gun_laws_by_nation), [business model](https://en.wikipedia.org/wiki/Uber#Legal_status_by_country), [cryptographic algorithm](https://www.eff.org/deeplinks/2017/07/australian-pm-calls-end-end-encryption-ban-says-laws-mathematics-dont-apply-down), [financial instrument](https://en.wikipedia.org/wiki/Credit_default_swap), [application of stochastic gradient descent](https://www.theguardian.com/technology/2017/jul/25/elon-musk-mark-zuckerberg-artificial-intelligence-facebook-tesla)

Finally two examples from computer science, one silly and one deadly serious.

- The [International Obfuscated C Code Contest](https://www.ioccc.org/) changes its rules after [an entry](http://www.ioccc.org/1992/nathan/index.html) was [illegal](https://en.wikipedia.org/wiki/Export_of_cryptography_from_the_United_States)

Every programmer has (if they even think about it at all) a mental model of their program in which certain rules cannot not be broken, in the sense of game theory. When the hacker broke the rules, they demonstrated that the programmer's mental model was not a useful model of the world.

[Dusko Pavlovic](https://dusko.org/) has an attractive approach to game theory based on computability theory which is intended to model arbitrary rule-breaks, with the intended purpose of creating models better-suited to computer security. However, his approach might have the potential to be more generally useful, by satisfying Kreps' wish for a more flexible approach to breaking the rules.[^2]

**Edit**: I gave a talk based on this blog post at Dusko's birthday conference, slides [here](https://www.cs.ox.ac.uk/people/julian.hedges/misc/Duskofest.pdf).
