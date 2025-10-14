---
title: Abusing the continuation monad
date: 22/09/2016
---

(Recall that [monads are not a good topic for your first blog post](https://wiki.haskell.org/What_a_Monad_is_not).)

I intend to bootstrap a blog by writing about 2 of my old papers, [Monad Transformers for Backtracking Search](https://cgi.cse.unsw.edu.au/~eptcs/paper.cgi?MSFP2014.3) and [The Selection Monad as a CPS Transformation](https://arxiv.org/abs/1503.06061). (Wall Street will be spared for the time being.)[^1]

[^1]: Editor's note: This post was written for a short-lived Wordpress blog I had called Obsolete Wall Street, which turned into [julesh.com](https://julesh.com) after the first post.

I'm going to write about this little bit of Haskell code:

```haskell
import Control.Monad.Cont
sat n = runCont $ sequence $ replicate n $ cont $ \k -> k $ k True
```

It's a [SAT solver](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem): you give it a boolean function, and the number of variables to search, and it decides whether that function will ever return true for any values of those variables.

How does this work? Haven't the foggiest. If anyone can explain what it does at runtime, there's probably a research paper in it for you. If you can also predict how long it takes, that's a big deal.

So where does this come from? It's a demonstration I wrote of the work of Martin Escardó and Paulo Oliva on selection functions. For Haskell programmers, good places to start are [Seemingly Impossible Functional Programs](https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) and [What Sequential Games, the Tychonoff Theorem and the Double-Negation Shift Have in Common](https://martinescardo.github.io/papers/msfp2010/). I removed all traces of selection functions, using only the continuation monad that we all know and love, to make it as short and magical-looking as I could.

The first bit of magic is the function `\k -> k $ k True`. Let's call it `e`, as in

```haskell
e k = k $ k True
```

This function `e` is an [existential quantifier](https://en.wikipedia.org/wiki/Existential_quantification). That is to say it takes a predicate (a function `k` into booleans), and decides whether there exists an input making the predicate true. For example, on natural numbers, applying an existential quantifier to the predicate `\n -> n^2 == 4` should return `True`, but applying it to `\n -> n^2 == 2` should return `False`.

The function `e` can't exactly do that. In fact, the existential quantifier for natural numbers isn't computable. What it does is to compute the existential quantifier for *booleans*.

I don't have a slick way to demonstrate this counter-intuitive fact. There's an awkward case-based argument, starting from the fact that `k True` is [either true, or it's false](https://en.wikipedia.org/wiki/Law_of_excluded_middle). But this is only ε more enlightening than just brute-forcing it: there are precisely 4 (total) functions `Bool -> Bool`, so it's easy to just evaluate `e` on each of them and check that it does the right thing.

So, we have the function `\k -> k $ k True :: (Bool -> Bool) -> Bool`. We hit that with `cont` which, despite not beginning with a capital letter, is the constructor of the `Cont` monad. So it doesn't actually do anything, except changing the type to `Cont Bool Bool`. (The `runCont` on the outside is the corresponding deconstructor, and also does nothing except change the type.)

Next we use `replicate n`, which creates a list of `n` identical copies. So at this point, we have a list containing `n` instances of the existential quantifier, each one wrapped inside `Cont`, the whole thing having type `[Cont Bool Bool]`.

The actual deep magic in the code snippet is the use of `sequence`, possibly the most magical function in Prelude.[^2] What `sequence` does, in general, is fold the monoidal product of a [monoidal monad](https://en.wikipedia.org/wiki/Monoidal_monad). For example, a useful little idiom in functional programming is to use `sequence` to find the cartesian product of lists (because cartesian product is the monoidal product of the list monad).

[^2]: Editor's note: When I wrote this, `sequence` had type `(Monad m) => [m a] -> m [a]`. Since writing it, its type in Prelude has been generalised to `(Foldable t, Monad m) => t (m a) -> m (t a)`.

It turns out that if you take the monoidal product of existential quantifiers in the `Cont` monad, you get one big existential quantifier for the product type. Which means, we get an existential quantifier for boolean lists of length `n`. Which is to say, if we give it a predicate `k :: [Bool] -> Bool`, it will decide if there is an input of length `n` making the predicate true. Which is another way of saying, it is a SAT solver for boolean functions with `n` inputs.

No, I'm not going to prove that here.

There are some pretty big open questions about this code I alluded to earlier, of which the biggest is that there doesn't seem to be any sort of complexity theory even capable of asking the question of what its runtime is. (The real difficulty is that `sequence` used this way is a third-order function, that is, its input is a function which itself takes a function as input.)

Instead, I'm going to ask a more manageable and concrete question: Can this code be written in continuation-passing style? By that I mean, can it be written in a way that doesn't use `cont`, but instead uses `call/cc`? Equivalently, can this code be translated to Scheme?[^3]

[^3]: Editor's note: After migration my blog does not support comments, but I received a comment anwering both of these by Jesse McDonald:

    The code is basically in continuation-passing style already:
    
    ```haskell
    sat n = runCont $ replicateM n $ callCC $ \k -> k =<< k True
    ```
    
    The only difference between this and the version with `cont` is that plain function application must be replaced with monadic bind; `cont` is just the `callCC` for `ContT r Identity` plus automatic lifting of the function into the `Identity` monad.
    
    This code fundamentally depends on the fact that `Cont` provides *delimited* continuations, bounded in scope by `runCont`. The equivalent in Racket Scheme using the `shift` and `reset` operators would be:
    
    ```scheme
    (require racket/control)
    (define (sat n f) (reset (f (for/list [(i (in-range 0 n))] (shift k (k (k #t)))))))
    ```
    
    There is a way to implement delimited continuations via undelimited continuations and mutation, but it isn't trivial. With just `call/cc` in Scheme you can't usefully write `(k (k #t))` because `k` represents the remainder of the entire *program*; the inner call would never return.
    
    As for runtime behavior, for each element in the input list this first inserts the value `True` or `#t`, due to inner `(k #t)`. The function is then applied to the resulting list, and the result (up to `runCont` or `reset`) becomes the input to the outer call to the continuation `(k (k #t))`. If `(k #t)` evaluates to `#t` then `(k (k #t))` reduces to `(k #t)` and finally just `#t`, which means that input remains "stuck" at `#t`; otherwise the function continues with the value `#f` in the manner of a normal nondeterministic depth-first exhaustive search.
    
    The function is always evaluated `2^n` times; there is no short-circuit evaluation. Replacing `(k (k #t))` with (or `(k #t) (k #f)`) gives the same result in fewer steps (unless the function only returns `#t` for all `#f` inputs) since it can immediately return `#t` as soon as the function produces `#t` for any input permutation and avoid calling the continuation again with the same input value.
    