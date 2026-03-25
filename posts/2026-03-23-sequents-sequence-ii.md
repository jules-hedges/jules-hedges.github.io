---
title: "Sequents for sequence II: Balancing the strangeness budget"
date: 23/03/2026
---

In [the previous post](/posts/2026-03-13-sequents-sequence.html) I explained how to build a sound system of proof rules for the tensor product and sequence product of polynomial functors using System L. This is better than not having a sound system of proof rules, but System L is an *extremely* strange language to program in, and I say this as one of a very small number of people with direct experience of it. I don't know a specific word for the style of programming that it is: it is declarative but definitely not functional. Since System L has a very well behaved operational semantics, it feels a bit like programming directly on [a very clean but very low level abstract machine](https://en.wikipedia.org/wiki/CEK_Machine).

Zanzi is developing a research language called Jermaine that leans into the strangeness, but I have different goals and I want to spend my language's [strangeness budget](https://steveklabnik.com/writing/the-language-strangeness-budget/) elsewhere. In this post I am going to balance my strangeness budget by transforming my calculus into an imperative programming language, with a *well typed elaborator* making the translation. 

The first thing I looked at was $\lambda\mu$-calculus, a hybrid system that removes coterms by transforming them into commands, resulting in a sort of half way point between a natural deduction calculus and a sequent calculus. This is close, but I still found it a bit strange, and while getting my head around exactly why it is strange I figured out the way I actually wanted to solve the problem.

## Computational $\lambda$-calculus

My final design is based on the [computational $\lambda$-calculus](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/moggi-computational-lambda-calculus-and-monads.pdf), the formal system behind Haskell's do-notation. It distinguishes pure *values* from *computations*, which live in a monad. I adapted this to work with a *graded monad*, specifically, the *graded writer* monad for the sequence product $T_p (a) = a \triangleright p$. Another closely related calculus is [fine grained call-by-value](https://www.sciencedirect.com/science/article/pii/S0890540103000889).

For conoisseurs of categorical cybernetics, the graded writer monad is an equivalent perspective on the [co-para construction](https://arxiv.org/abs/2105.06332), and so this can be seen as a calculus for lenses that are right-coparametrised with the sequence product.

We are then going to build in an algebraic signature for this monad, which turns out to be the theory of a *stack*: we will have primitives to push and pop values.

We start out with *expressions*, which don't interact with the stack at all:

```idris
  data Expression : Inputs -> Ty -> Type where
    Var : Expression [<a] a
    UnitIntro : Expression [<] Unit
    TensorIntro : Partition as1 as2 as
      -> Expression as1 a -> Expression as2 b
      -> Expression as (Tensor a b)
```

Next, we have *programs*, which correspond to sequences of imperative instructions, possibly ending with a return value which is an expression. Later it will turn out to be helpful to unify them into a single datatype, but to keep things clear for now let's keep things separate.

It turns out to be convenient to define programs by post-composing instructions. Let's first write the rules that don't involve binding, which are simpler. The primitive instructions are going to be

- `push`, which pushes one value onto the stack
- `pop0`, which pops zero values from the stack in the form of a `Unit`
- `pop1`, which pops one value from the stack
- `pop2`, which pops two values from the stack in the form of a `Then`

```idris
data Program : Inputs -> Outputs -> Type where
```

- `{}` is a program with no return value
```idris
  Noop : Program [<] []
```

- If `p` is a program with no return value and `e` is an expression, then `p; push(e)` is a program with no return value
```idris
  Push : Partition as1 as2 as
    -> Program as1 bs -> Expression as2 b
    -> Program as (b :: bs)

data ProgramRet : Inputs -> Ty -> Outputs -> Type where
```

- If `p` is a program with no return value and `e` is an expression, then `p; e` is a program with a return value (here I am writing a Rust-like syntax where the return value is a naked expression that is not enclosed by a `return` control operator)
```idris
  Pure : Partition as1 as2 as
    -> Program as1 bs -> Expression as2 b
    -> ProgramRet as b bs
```

- If `p` is a program with no return value, then `p; pop0()` and `p; pop1()` and `p; pop2()` are programs with a return value
```idris
  Pop0 : Program as bs
    -> ProgramRet as Void bs
  Pop1 : Program as (b :: bs)
    -> ProgramRet as b bs
  Pop2 : Program as (a :: b :: bs)
    -> ProgramRet as (Then a b) bs
```

By making a distinction between expressions and programs with a return value, we are making a Haskell-like stratification into pure and effectful values. When we have a program with a return value `p; e`, the syntax `e` could be an expression, but it could also be a `pop` which is not considered an expression. In particular, we are not allowed to write things like `Tensor Pop1 Pop1`. Of course it is entirely possible to give a sensible semantics to such an expression by choosing a traversal ordering, and most real world languages do exactly this, but such a language can easily desugar to one that makes the stratification.

## Matching and case splits

If we were to write an *elaborator* that translates this language into System L, we would find that programs with no return value correpond to commands, and programs with a return value correspond to terms. What we are missing is any way of expressing the rules that produce coterms. This is where case splits come in.

A `Cases` corresponds to the body of a construct such as `match(e)` or `case e of`. Like programs, cases can either return a value or not. This gives us a calculus with 4 judgements: while System L has judgements with no focus, an output focus and an input focus, we also have a judgement with both an input and an output focus, corresponding to a returning case split that focuses both its scrutinee and its return value.

At this point we can productively merge pairs of judgements together using a `Maybe Ty` for the output type, saving us some duplication of rules. (This is a trick that Zanzi uses very heavily in her work.) I will write all of the rules for programs again since they have slightly changed, but only comment the one that is new.

```idris
data Program : Inputs -> Maybe Ty -> Outputs -> Type where
  Noop : Program [<] Nothing []
  Pure : Partition as1 as2 as
    -> Program as1 Nothing bs -> Expression as2 a
    -> Program as (Just a) bs
```

- If `p; e` is a program with a return value (noting that `e` could be either an expression or a pop), and `c` is the body of a case split, then `p; case e of c` is a program, which has a return value if the case split does

```idris
  CaseOf : Partition as1 as2 as -> Simplex bs2 bs1 bs
    -> Program as1 (Just a) bs1 -> Cases as2 a b bs2
    -> Program as b bs
  Push : Partition as1 as2 as
    -> Program as1 Nothing bs -> Expression as2 b
    -> Program as Nothing (b :: bs)
  Pop0 : Program as Nothing bs
    -> Program as (Just Unit) bs
  Pop1 : Program as Nothing (b :: bs)
    -> Program as (Just b) bs
  Pop2 : Program as Nothing (a :: b :: bs)
    -> Program as (Just (Then a b)) bs

data Cases : Inputs -> Ty -> Maybe Ty -> Outputs -> Type where
```

- If `p` is a program with a free variable `x` then `x => p` is the body of a case split whose scrutinee is the type of `x`, which has a return value if the program does

```idris
  Let : Program (as :< a) b bs
    -> Cases as a b bs
```

- If `p` is a program then `Unit => p` is the body of a case split whose scrutinee is of type `Unit`, which has a return value if the program does

```idris
  LetUnit : Program as b bs
    -> Cases as Unit b bs
```

- If `p` is a program with free variables `x` and `y` of types `a` and `b`, then `Tensor x y => p` is the body of a case split whose scrutinee has type `Tensor a b`, which has a return value if the program does

```idris
  LetTensor : Program (as :< a :< b) c bs
    -> Cases as (Tensor a b) c bs
```

- This last one is mysterious.

```idris
  ThenMatch : Partition as1 as2 as -> Simplex bs2 bs1 bs
    -> Cases as1 a Nothing bs1 -> Cases as2 b c bs2
    -> Cases as (Then a b) c bs
```

The `ThenMatch` rule corresponds to `ThenCointro` in System L, and is similarly hard to understand. If we case split on a scrutinee of type `Then a b` then we split into two branches, one of which gets the `a` and the other gets the `b`. So far, this is like matching on a sum type `Either a b`, where we have `Left x` and `Right y` branches.

I can imagine similar tags `First` and `Second`, so we have a syntactic form where if `p` is a program with a free variable `x` of type `a`, and `q` is a program with a free variable `y` of type `b`, then
```
{First x => p; Second y => q}
```
is the body of a case split whose scrutinee has type `Then a b`.

What gives away the strangeness is the linear scoping rules. When we case split on a sum type only one branch actually executes and the other does not, and so if we are in a linear world then both branches must consume every linear variable. But this rule uses a `Partition` for inputs and a `Simplex` for outputs, which means that *both* branches execute, in order: if we count usages, every linear variable is used by either the first branch or the second branch, and so if both branches run then every linear variable ends up getting consumed once. This combines aspects of matching on a tensor product and matching on a sum.

The tags `First` and `Second` should not be confused with the *projections* from a cartesian product, although they are intentionally named in a similar way. The `First` branch runs first, and the `Second` branch runs second. It turns out that the first branch may not have a return value, and the overall case split has a return value if the second branch does. The very interesting thing is that both branches can push and pop, which should allow us to *smuggle* information from the first branch to the second branch via the stack.

## The Polylang project

I had intended to end with examples of things you can do in this language, but for several reasons I am leaving it for a future post, starting with that this one has already got longer than I intended. (This post and the previous one already started out as a single post that got split in half.)

The *honest* reason for not writing the last section yet is that I'm myself still in the process of learning what programming in this language is like. But that itself is for multiple reasons. The biggest practical barrier is that programming in a well typed language with de Bruijn indices is a huge pain. This is overcome by writing a scopechecker and typechecker that infer the partitions and simplices, and less importantly also a parser. But it turns out that scopechecking and typechecking System L is also [a research topic in its own right](https://arxiv.org/abs/2512.07511).

Another reason is that the language fragment containing just tensor product and sequence product is quite limited in expressive power. Adding coproducts is easy (except for scopechecking them, which is extremely hard), but adding cartesian products requires a complex system of *quantities* inspired by [quantitative type theory](https://bentnib.org/quantitative-type-theory.pdf). This was worked out by [Dylan](https://dylanb.me/index.xml) and will be the topic of at least one future post by either him or me.

And now for the reveal: At [Glaive](https://glaive-research.org/) we are developing a general purpose programming language around these ideas. Its working name is **polylang**, and the name has stuck enough that I want to name the language **Poly**.

The applications of polylang are exactly the applications of polynomial functors and lenses, which are *very* numerous. The first application that are developing is for implementing typed tactics for Lean as part of [our grant](https://www.renaissancephilanthropy.org/a-structured-representation-of-tactics-for-machine-assisted-theorem-proving), and the importance of the sequence product for tactics (where it describes actual sequencing of tactics) was the single biggest inspiration for the overall design.

Other use cases for polylang that we have in mind are precisely the applications of [categorical cybernetics](https://cybercat.institute/2022/05/29/what-is-categorical-cybernetics/), which include (but are not limited to!) [statically typed autodiff](https://arxiv.org/abs/2103.01931), [reinforcement learning](https://arxiv.org/abs/2404.02688), [Bayesian learning](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.MFCS.2023.24), [compositional game theory](https://github.com/CyberCat-Institute/open-game-engine/), and the trifecta of full stack programming: [frontend](https://cybercat.institute/2025/01/21/ui-para-optic/), [backend](https://arxiv.org/abs/2203.15633) and [databases](https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/ssbx-intro.pdf).

There is nothing to release yet, nor can I give a release date, but we are likely to make a public release of an early version this year. Language development and compiler development are very hard, and we are not making things any easier for ourselves by using an experimental methodology, making as much as possible well typed and using Idris as an implementation language. Watch this space!

## Appendix: The well typed elaborator

The term *elaborator* means roughly a glorified desugarer; it is normally used with dependently typed language to refer to the translation from patterns to elimination forms. I have taken to using it more generally to refer to any compiler pass that translates between representations that belong to fundamentally different language families, which is what this is.

The code of the elaborator is not extremely enlightening, but I am including it essentially as a reference for how the two languages map onto each other. We quietly get a payoff for defining partitions in a very specific way in the previous post, since the constructor `[<]` defines a partition in which every variable takes the left branch, which we do repeatedly.

```idris  
elabExpression : Expression as a -> Term as a []
elabExpression Var
  = Var
elabExpression UnitIntro
  = UnitIntro
elabExpression (TensorIntro pa t1 t2)
  = TensorIntro pa Z (elabExpression t1) (elabExpression t2)

mutual

  elabProgram : Program as Nothing bs -> Command as bs
  elabProgram Noop
    = mix0
  elabProgram (CaseOf pa sx t1 t2)
    = Cut pa sx (elabProgramRet t1) (elabCases t2)
  elabProgram (Push pa t1 t2)
    = mix pa (S Z)
        (elabProgram t1)
        (Cut [<] (S Z) (elabExpression t2) Covar)

  elabProgramRet : Program as (Just b) bs -> Term as b bs
  elabProgramRet (Pure pa t1 t2)
    = Out (mix pa (S Z)
             (elabProgram t1)
             (Cut [<] (S Z) (elabExpression t2) Covar))
  elabProgramRet (CaseOf pa sx t1 t2)
    = Out (Cut pa (S sx)
             (elabProgramRet t1)
             (elabCasesRet t2))
  elabProgramRet (Pop0 t)
    = UnitComatch (elabProgram t)
  elabProgramRet (Pop1 t)
    = Out (elabProgram t)
  elabProgramRet (Pop2 t)
    = ThenComatch (elabProgram t)

  elabCases : Cases as a Nothing bs -> Coterm as a bs
  elabCases (Let t)
    = In (elabProgram t)
  elabCases (LetUnit t)
    = UnitMatch (elabProgram t)
  elabCases (LetTensor t)
    = TensorMatch (elabProgram t)
  elabCases (ThenMatch pa sx t1 t2)
    = ThenCointro pa sx (elabCases t1) (elabCases t2)

  elabCasesRet : Cases as a (Just b) bs
    -> Coterm as a (b :: bs)
  elabCasesRet (Let t)
    = In (Cut [<] (S Z) (elabProgramRet t) Covar)
  elabCasesRet (LetUnit t)
    = UnitMatch (Cut [<] (S Z) (elabProgramRet t) Covar)
  elabCasesRet (LetTensor t)
    = TensorMatch (Cut [<] (S Z) (elabProgramRet t) Covar)
  elabCasesRet (ThenMatch pa sx t1 t2)
    = ThenCointro pa (S sx) (elabCases t1) (elabCasesRet t2)
```
