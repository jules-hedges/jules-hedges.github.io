---
title: A first look at programming in Poly
date: 26/06/2026
---

I ended [my last post](/posts/2026-03-23-sequents-sequence-ii.html) with a teaser for what I have been working on for almost the last year now: a programming language called Poly in which types denote containers (aka polynomial functors) and functions denote lenses. It is an imperative language based on a computational $\lambda$-calculus for the noncommutative graded monad $T_p (a) = a \triangleright p$, whose algebraic theory is that of a stack.

The compiler is still far from ready for a public release, but I'm still optimistic for a release this year. This post is part teaser and part update, where I will describe what has been done and a bit of the development roadmap. I want to show off a case study for a nontrivial program that the compiler is (almost) able to handle successfully: the "minimum" binary operation on the container of finite sets. This is a container that nontrivially involves dependent types, and yet Poly itself is simply-typed, so an accurate title for this post could be "Dependent types without dependent types, with dependent types".

## The state of Polylang 

Here is a quick summary of the major components of the compiler Polylang, in almost reverse order:

- An intrinsically well typed core language based on a System L with folded tensor product on the left and folded sequence product on the right
- An intrinsically well typed interpreter from Core to Idris lenses
- A structurally recursive and intrinsically sound single-pass scopechecker and typechecker for System L, based on [our paper](https://arxiv.org/abs/2512.07511) and many extensions that Zanzi and me have developed and not written down yet, particularly for the very difficult task of scopechecking in the presence of additive units (more on this later)
- An elaborator from the imperative surface language to System L following the design in my previous post
- A step called the "desugarer" that transforms raw abstract syntax into the post-composition order required by the elaborator
- Last and least, a parser which could charitably be described as "existing" but which throws a major wobbly if you put a single character out of place

Now I will describe all of the features currently supported, and the features that are partially implemented or planned.

- Notably, currently all variables are linear. Most of the time this means that every variable in scope must be referenced exactly once, although it gets far more subtle in the presence of additives (cartesian products and products, and especially their units). I have a mostly but not completely implemented system of quantities allowing variables to become cartesian when they are under a linear exponential modality. I also plan a system where linear variables can be "demoted" and used multiple times at lower quantity, allowing for example implementation of the two natural lenses $a \to a \otimes a$.

- Binary and nullary tensor products, which have types, values and patterns all using the syntax `(x, y)` and `()`. For example, here is the concrete syntax for associativity of the tensor product:
```
function assoc (x : ((nat, nat), nat)) : (nat, (nat, nat)) {
  (x12, x3) = x;
  (x1, x2) = x12;
  (x1, (x2, x3))
}
```
Later this will be extended to unbiased $n$-ary tensor products. (The previous code snippet reveals that I still have no idea how to handle nested patterns.)

- Binary and nullary sequence products, which I talked about enough in my previous post that I'm not going to say any more about them in this post.

- Binary coproducts, currently using the syntax `|` for types, and `Left x` and `Right y` for values and patterns. For example, associativity of coproducts looks like this:
```
function assoc (x : (nat | nat) | nat) : nat | (nat | nat) {
  match (x) {
    Left x12 => match (x12) {
      Left x1 => Left x1,
      Right x2 => Right (Left x2)
    },
    Right x3 => Right (Right x3)
  }
}
```
Since only one branch of a coproduct match ever runs, every branch must consume the same set of linear variables - the first hint that linearity is going to get subtle. So far I am considering the nullary coproduct to not be useful enough to be worth the difficulty of scopechecking it. Later I expect to subsume all of this syntax into a general purpose mechanism for algebraic datatypes. (Poly is not whitespace-sensitive, which happens to be my personal preference, but I consider that concrete syntax is essentially trivial to change later.)

- Binary cartesian products. A binary product is eliminated by projection, with the syntax `x.first` and `x.second`. Notably projection counts as a use of the linear variable `x`, so it is not possible to extract both branches of a product. Binary products are creating using the syntax `{x, y}`. Since only one branch will ever be extracted, both branches must consume the same set of linear variables. So swap for a cartesian product is written `{x.second, x.first}` and that constitutes only a single consumtion of `x`.

- The nullary product, ie. the terminal object, which is created using the syntax `{}`. There is no general way to eliminate it: once you have a variable of type `{}` in scope you are often stuck with it permanently. Since there are very few lenses out of the cartesian unit (since the backwards pass must return an element of the empty set) this works as intended to prevent writing programs that could have no denotation. But much more subtle is the fact that creating the value `{}` can in general consume arbitrary linear variables, since it is the terminal object. This gives the scopechecker the difficult task of figuring out which variables must be consumed by `{}` because they are not consumed elsewhere. Doing this in a structurally recursive way was interesting enough that it will probably be the topic of a paper rather than a blog post. In theory `{}` should also be able to *produce* arbitrary values on the output stack, but this is currently not implemented because I still have no idea how to scopecheck it without help from a syntactic annotation.

- Toplevel functions, currently restricted to at most 1 input and at most 1 output (but multiple inputs and outputs can be easily simulated using the tensor and sequence products respectively.) Toplevel functions can be recursive, in an unrestricted way, with no form of termination checking planned - this is the only intended failure of soundness in the language. Right now I have an embarassing bug where toplevel recursion causes an exponential blowup in the interpreter, so that I can calculuate $2 + 2$ with unary naturals but it takes about 20 seconds.

- IO is currently hacked in for demonstration purposes, but we have half-formed plans for a custom algebraic effect system making heavy use of the sequence product.

- By far the biggest piece of work to be done, which I expect to take the next several months, are the related tasks of adding parametric type variables (aka generics) and a general system for algebraic datatypes. Right now I have several specific recursive datatypes hacked in. For example, the type called `nat` (which I call the "flat naturals"), is the container whose forwards set is $\mathbb N$ and backwards set is $1$, or as a polynomial functor $\mathbb N y$. It can be defined "intrinsically" as the least fixpoint of `nat | ()`, or as a polynomial functor as the least fixpoint of $N (y) = y + N (y)$. It is defined by two constructors `Zero` and `Succ n`, and a deconstructor that consumes a `nat` and produces a `() | nat`, which can then be matched on. A much more interesting example will be the topic of the next section.

## The Fin container

Now I'm going to talk through a nontrivial program in Poly. $\mathbf{Fin}$ is the container whose forwards set is $\mathbb N$, and whose backwards set over $n$ is a finite set with $n$ labelled elements. As a polynomial functor this is the power series $F (y) = \sum_{n = 0}^\infty y^n$, which is naturally isomorphic to $\mathbf{List} (y)$. For this reason $\mathbf{Fin}$ is usually called the $\mathbf{List}$ container, but I'm choosing to call it $\mathbf{Fin}$ because this really isolates the difference between "Set-centric thinking" and "Poly-centric thinking": $\mathbf{Fin}$ is used to define lists in the category of sets, but the category of containers has list monads of its own: André wrote about them [here](https://glaive-research.org/2026/03/18/bootstrapping-data-structures.html).

Normally the $\mathbf{Fin}$ container requires dependent types to define. For example in Idris, the fibres are defined like this:
```idris2
data Fin : Nat -> Type where
  FZ : Fin (S n)
  FS : Fin n -> Fin (S n)
```
In Poly, we can define $\mathbf{Fin}$ intrinsically without requiring any use of dependent types: it is the least fixpoint of `Fin = {} | {(), Fin}`. In terms of polynomial functors this is the standard telescoping recurrence for the power series, $F (y) = 1 + y F (y)$ (this is how Euler would do containers, radius of convergence is for cowards). As I said earlier I don't have a general mechanism for defining recursive datatypes in Poly yet, but my idealised syntax is something like
```
data Fin = FinZero {} | FinSucc {(), Fin}
```

Let's write a nontrivial program using `Fin`, and talk through in detail how its scoping works. There is a lens $\min : \mathbf{Fin} \otimes \mathbf{Fin} \to \mathbf{Fin}$ which takes the minimum of two natural numbers in the forwards pass, and in the backwards pass embeds a finite set with `min m n` elements into finite sets with `m` and `n` elements. In terms of extensions $\mathbf{Fin} \otimes \mathbf{Fin}$ is the endofunctor that sends a set $A$ to the set of (rectangular) matrices of $A$, and the extension of the lens $\min$ is the natural transformation from the matrix functor to the list functor that takes the diagonal starting from top left and stops when it hits the shortest dimension.

Our starting point is the recursive definition of binary minimum of natural numbers, which in awkwardly verbose Idris  looks like this:
```
min : Nat -> Nat -> Nat
min m n = case m of
  Z => Z
  S pm => case n of
    Z => Z
    S pn => S (min pm pn)
```
(Here `pm` is mnemonic for "predecessor of `m`".) We can write the minimum operator for $\mathbf{Fin}$ in Poly by following the same approximate structure, first matching on the first input and then in the successor case matching on the second input.

We start like this:
```
function min (m : fin, n : fin) : fin {
  match (m) {
    FinZero zm => ?,
    FinSucc pm => ?
  }
}
```
In the first branch we get a variable `zm : {}`, the cartesian unit. As I said earlier, normally this is bad because there is no general way to get rid of a cartesian unit in scope. But in this branch we want to return the zero constructor, and that itself takes a cartesian unit as a parameter. So we can just use `zm` as the parameter, but there's also something else we can do: we can just write `{}` to introduce a cartesian unit, and the scopechecker will figure out that by creating the `{}` we must be consuming the variable `zm` because there is nowhere else for it to go.

```
function min (m : fin, n : fin) : fin {
  match (m) {
    FinZero zm => FinZero {},
    FinSucc pm => match (n) {
      FinZero zn => ?,
      FinSucc pm => ?
    }
  }
}
```

In the right branch of the match on `m` we gain `pm : {(), Fin}`, the cartesian product of a tensor unit and a `Fin`. The next thing we need to do is to match on `n`, and in its zero branch we need to return the zero constructor, while consuming both `pm` and `zn`. Once again we can do this by writing `FinZero {}` since the `{}` consumes anything we need it to.

In the final branch we need to make a recursive call and wrap it in a `FinSucc` constructor. We have `pm` and `pn` both of type `{(), Fin}`, and we can take their right projections to get the inputs to the recursive call. What we would *like* to write is this:
```
      FinSucc pm => FinSucc {(), min (pm.right, pn.right)}
```
But this doesn't scopecheck: both branches of the cartesian product must consume the same variables, but the left branch `()` doesn't consume anything. What we need to do in the left branch is to left-project out of `pm` and `pn` and pattern match the resulting tensor units to consume them. This can be written
```
function min (m : fin, n : fin) : fin {
  match (m) {
    FinZero zm => FinZero {},
    FinSucc pm => match (n) {
      FinZero zn => FinZero {},
      FinSucc pm => FinSucc {
        let ((), ()) = (pm.left, pn.left) in (),
        min (pm.right, pn.right)
      }
    }
  }
}
```

This is the point where I admit that I can't *quite* compile this program yet. I didn't anticipate that the subtle interaction of linearity and cartesian products would make it a hard requirement to write non-value terms in value position (aka the Moggi transformation), so I put it off for the future as a mere quality of life feature. For my first attempt I was working off a buggy version of the original Idris `min` function that omitted the successor constructor in the final case, making it a constant zero function with extra steps (the extension of the corresponding lens takes every matrix to the empty list). That function *can* be handled by my compiler right now, and I will show what it looks like with all of the syntactic workarounds I am currently working with:
```
function min (mn : (fin, fin)) : fin {
  (m, n) = mn;
  dm = deconsFin (m);
  dn = deconsFin (n);
  match (dm) {
    Left zm => FinZero {},
    Right pm => match (dn) {
      Left zn => FinZero {},
      Right pn => {
        pmr = pm.right;
        pnr = pn.right;
        min ((pmr, pnr))
      }
    }
  }
}
```

## The road ahead

All of this was intended as a teaser, to give an idea of what I've already built and what it can already handle, and what is still to be done. The point of the last section is to give an impression of what it's like to program *intrinsically* with containers and lenses. Of course Poly is actually intended not for this kind of category-theoretic exercise but for actually practical programming tasks in categorical cybernetics - the one I'm prioritising because of our grant is implementing typed tactics, so more on that in future.

My big task for the next few months is going to be parametric polymorphism and user-definable datatypes, which will be a huge step up in expressive power. In particular, this will make it possible to define the three list monads: one each for tensor product, cartesian product and sequence product. The latter of these, lists for the sequence product, is equivalent to the *free monad monad* on the category of polynomial functors, an incredibly useful construction that shows up constantly in the world of tactics but also in many other places, probably including the hypothetical future implementation of effects.

The other thing I haven't mentioned at all is anything at the back end of the compiler. Aside from the Lean target this is a problem I'm leaving for myself in the future because I really know nothing about that side of things, but my intention is to target either Javascript or WASM (whichever turns out to be easier) implementing the backwards pass using the same mechanism as is used for callbacks.
