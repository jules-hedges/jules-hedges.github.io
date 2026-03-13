---
title: Sequents for sequence
date: 13/03/2026
---

It's time for me to reveal what I've been working on since last summer. Early last year I was working on a programming language in which types denote pairs of sets and functions denote lenses, which I wrote about in a series of posts: [I](/posts/2024-08-26-bidirectional-programming-i.html), [II](/posts/2024-09-05-bidirectional-programming-ii.html), [III](/posts/2024-09-12-bidirectional-programming-iii.html), [IV](/posts/2025-01-03-bidirectional-programming-iv.html). I am still doing that, but with different design constraints caused me to throw the original design away and start over. To be specific, that language started from the principle that the semantic category would be *simply typed* lenses, and I leaned deeply into the one piece of structure I know that is specific to that category: the non-functorial but very interesting "negation" operation of swapping the two sets. What is given up for that is that the language couldn't properly support coproducts or branching.

After learning more about the structure of lenses, I eventually became convinced that this was not a good tradeoff, and so I started over with a slightly different semantics in mind: types denote [containers](https://people.cs.nott.ac.uk/psztxa/publ/fossacs03.pdf) and functions denote dependent lenses, or equivalently, types denote [polynomial functors](https://toposinstitute.github.io/poly/poly-book.pdf) and functions denote natural transformations.

The that changed was that I quit academia and joined [Glaive](https://glaive-research.org/) to work on their grant on [structured tactics](https://www.renaissancephilanthropy.org/a-structured-representation-of-tactics-for-machine-assisted-theorem-proving) for Lean, allowing me to work full time on the task of developing the language that I had been missing for years, the next generation [Open Game Engine](https://github.com/CyberCat-Institute/open-game-engine). Previous work by [André](https://andrevidela.com/) and [Bob Atkey](https://bentnib.org/) reveals that tactics can be seen as lenses, adding tactics to the increasingly long list of interesting things that could be done with this hypothetical language.

One interesting complication is that tactics-as-lenses makes heavy use of a structure on lenses that I had never worked with before: the *sequence product*, also called the *composition product*, a very strange noncommutative monoidal product that exists on both simple and dependent lenses. In the world of tactics it describes actual sequencing: first apply one tactic, then another, where the second has access to the result of the first.

The sequence product turned out to be so difficult to understand that it warps the entire design of the language around it. The language I am developing has a lot of moving pieces, far too many to explain in one post, so in this post I am going to focus on how we tamed the sequence product, focussing on its interaction with the tensor product. Even this will be split across two posts, with the theoretical solution this post and exploring its practical implications in a sequel. The next thing to add after that is cartesian products, but that requires a whole other collection of machinery that can wait for a future post.

## The tensor product

Just so we start out in the same book, I will quickly explain again how to make a language for just tensor products, which is the one thing that remains somewhat in common with my old design. I think of it as a linear $\lambda$-calculus but where I'm omitting function types for simplicity. We start out with a language for types:

```idris
data Ty : Type where
  Unit : Ty
  Tensor : Ty -> Ty -> Ty

Inputs : Type
Inputs = SnocList Ty
```

We now need to represent contexts, that is lists of inputs variables, which are linear but commutative. There are several ways to do this, but this is how I am going to do it here; this is not the easiest way but will save me a minor headache in the next post. The type `Partition` describes how variables to be split arbitrarily between two sub-contexts but never allowing any variable to go into both.

```idris
data Insertion : Inputs -> Ty -> Inputs -> Type where
  Here : Insertion as a (as :< a)
  There : Insertion as a as'
    -> Insertion (as :< b) a (as' :< b)

data Partition : Inputs -> Inputs -> Inputs -> Type where
  Lin : Partition as [<] as
  (:<) : Partition as bs cs -> Insertion bs b bs'
    -> Partition as bs' (cs :< b)
```

Now we can write our language of well typed terms:

```idris
data Term : Inputs -> Ty -> Type where
  Var : Term [<a] a
  Let : Partition as bs cs -> Term as a
    -> Term (bs :< a) b -> Term cs b
  UnitIntro : Term [<] Unit
  LetUnit : Partition as bs cs -> Term as Unit
    -> Term bs a -> Term cs a
  TensorIntro : Partition as bs cs
    -> Term as a -> Term bs b -> Term cs (Tensor a b)
  LetTensor : Partition as bs cs -> Term as (Tensor a b)
    -> Term (bs :< a :< b) c -> Term cs c
```

This language can be equipped with a well typed interpreter that interprets types as containers and terms as dependent lenses, where the context is interpreted as a folded tensor product. It begins like this:

```idris
Fw : Ty -> Type
Fw Unit = Unit
Fw (Tensor a b) = (Fw a, Fw b)

Bw : (a : Ty) -> Fw a -> Type
Bw Unit () = Unit
Bw (Tensor a b) (x, y) = (Bw a x, Bw b y)
```

The semantics of a term is a dependent lens from the tensor product of its inputs to its output. I'm not going to cover that in this post because it gets a bit gnarly for the sequence product, so you will have to trust me that everything in this post and the next is sound for the category of dependent lenses (and it happens to also be sound for the category of simple lenses too).

## The sequence product

We extend our type language with the sequence (aka composition) product, which in code I call `Then` and in symbols is usually called $\triangleright$. It is just another binary operator on the type language:

```idris
data Ty : Type where
  Unit : Ty
  Tensor : Ty -> Ty -> Ty
  Then : Ty -> Ty -> Ty
```

Its semantics looks very strange at first:

```idris
mutual

  Fw : Ty -> Type
  Fw Unit = Unit
  Fw (Tensor a b) = (Fw a, Fw b)
  Fw (Then a b) = (x : Fw a ** Bw a x -> Fw b)
  
  Bw : (a : Ty) -> Fw a -> Type
  Bw Unit () = Unit
  Bw (Tensor a b) (x, y) = (Bw a x, Bw b y)
  Bw (Then a b) (x ** k) = (x' : Bw a x ** Bw b (k x'))
```

Here `**` is the type and term former for $\Sigma$-types (aka dependent pairs) in Idris. By convention `->` binds tighter than `**`.

This is a very strange looking definition when written like this in terms of containers, but in the equivalent world of polynomial functors it is extremely natural. The mapping from containers to polynomial functors is called "extension" and is given on objects like this:

```idris
extension : (a : Type) -> (a' : a -> Type) -> Type -> Type
extension a a' y = (x : a ** a' x -> y)
```

The sequence product has the property that extension carries it to ordinary functor composition:

```idris
extension (Fw (Then a b)) (Bw (Then a b))
=== extension (Fw a) (Bw a) . extension (Fw b) (Bw b)
```

Readers paying enough attention might have noticed that my name `Then` appears to be backwards, because the functor composition `.` is "after". This is a subtlety I don't fully understand yet, but I have had a lot of success treating the extension as though it is contravariant for sequencing: it sends container sequencing in diagrammatic order to functor composition in algebraic order. This is extremely convenient because it means I've been able to work with diagrammatic order sequencing without having to fight against established conventions.

Last summer I started out naively trying to find logical rules for the sequence product. I spent a month throwing everything I had at the problem, and nothing I tried worked. I don't know of a conceptual argument why it must be impossible to combine together the tensor and sequence products in a natural deduction calculus, but it does seem to be impossible, at least without using dependent types.

(In the meantime [Dylan Braithwaite](https://dylanb.me/index.xml) has been simultaneously exploring a completely different region of the design space describing sequencing with a [cohensive dependent type theory](https://ncatlab.org/nlab/show/cohesive+homotopy+type+theory), but dependent types hurt my head so I will let him write about that.)

Let's talk about the strange properties of the sequence product. Firstly it is a monoidal product: it is associative, and it turns out that its unit is the same as the unit of the tensor product (whose extension is the identity functor). Of course it's not commutative, since functor composition isn't.

It has *left* distributive laws with the cartesian product and coproduct, which expresses that cartesian products and coproducts of functors are pointwise. This won't come up in this post, but is still important.

The interaction with the tensor product is very strange and complicated, and I am extremely indebted to the paper [What kind of linear distributive categories do polynomial functors form?](https://arxiv.org/abs/2407.01849) by David Spivak and Priyaa Srinivasan for finding the right way to think about it.

The tensor and sequence products interact with a structure called a *duoidal category* that makes them behave as though they are the tensor product and morphism composition of a symmetric monoidal category: there is a canonical "interchangor" lens $(a \triangleright b) \otimes (c \triangleright d) \to (a \otimes c) \triangleright (b \otimes d)$

The logics of duoidal categories go by the name [deep inference calculi](https://en.wikipedia.org/wiki/Deep_inference) and are so complicated that I consider them completely unworkable right now. Fortunately, Spivak and Srinivasan proved that every duoidal category in which the two tensor units coincide is also a linearly distributive category: there is a canonical lens $a \otimes (b \triangleright c) \to (a \otimes b) \triangleright c$.

## Sequents for sequence

The knowledge that tensor product and sequence product satisfy linear distributivity was the knowledge that [Zanzi](https://zanzix.github.io/) needed to tell me what I needed to do. Linear distributive categories can be equipped with syntaxen based on *sequent calculi* rather than natural deduction calculi: that is, we want both an input context and an output context. Specifically in this case, we want the input context to be interpreted by the tensor product and the output context to be interpreted by the sequence product. This puts us in a probably unique middle ground of logic, where we are linear but commutative on the left and fully noncommutative on the right:

Under Zanzi's tuition I built not a standard sequent calculus but a System L (aka $\mu\tilde\mu$-calculus), an equivalent presentation that is considered better behaved for programming, removing the nondeterminism of cut elimination that connects sequent calculus to concurrency. (This is the sequence of unlikely events that led to me coauthoring [our paper on typechecking System L](https://arxiv.org/abs/2512.07511).)

System L has 3 classes of judgements: *commands* which are like ordinary sequents, *terms* which have an additional focussed output, and *coterms* which have an additional focussed input. Since we need to focus into the noncommutative output context we face a very nontrivial choice up front: do we focus on the left, on the right, or have a movable focus using a zipper? Luckily it turns out that the best choice is to be left-focussed, as a consequence of essentially all of the rules for the sequence product being left-biased.

```idris
Outputs : Type
Outputs = List Ty

data Simplex : Outputs -> Outputs -> Outputs -> Type where
  Z : Simplex [] bs bs
  S : Simplex as bs cs -> Simplex (a :: as) bs (a :: cs)
```

Here is what a minimal System L looks like with no logical rules:

```idris
mutual

  data Command : Inputs -> Outputs -> Type where
    Cut : Parition as1 as2 as -> Simplex bs2 bs1 bs
      -> Term as1 ty bs1 -> Coterm as2 ty bs2
      -> Command as bs

  data Term : Inputs -> Ty -> Outputs -> Type where
    Var : Term [<a] a []
    Out : Command as (b :: bs) -> Term as b bs

  data Coterm : Inputs -> Ty -> Outputs -> Type where
    Covar : Coterm [<] b [b]
    In : Command (as :< a) bs -> Coterm as a bs
```

In System L, terms are *producers* that tell us how to construct an output value of the focussed type, and coterms are *consumers* that tell us how to deconstruct an input value of the focussed type. Semantically, `Term as b bs` is the same as `Command as (b :: bs)`, and `Coterm as a bs` is the same as `Command (as :< a) bs`. The `Out` and `In` rules are $\lambda$-like binders, normally called $\mu$ and $\tilde\mu$ in the literature, which bring an output or an input into focus respectively.

The cut rule is like the one from sequent calculus except we can only cut a focussed output with a focussed input. It is an important principle of Systen L that `Cut` is the *only* way to form a command: every other rule is either a term or a coterm. Readers paying enough attention might have noticed that the `Simplex` for cuts is backwards: this is a semantic consequence of noncommutativity of the sequence product combined with my choice to make the output context into a cons list, growing from right to left. That is, the term "runs" first and so its outputs go further right on the output stack.

In System L, logical rules are split into introduction rules, which either combine terms or coterms depending on the natural polarity of the connective, and match rules, which bind either inputs or outputs of a command. The tensor product is positive, which means that its introduction rule combines terms, and its match rule binds inputs and produces a coterm.

```idris
data Term : Inputs -> Ty -> Outputs -> Type where
  UnitIntro : Term [<] Unit []
  TensorIntro : Partition as1 as2 as -> Simplex bs1 bs2 bs
    -> Term as1 a bs1 -> Term as2 b bs1
    -> Term as (Tensor a b) bs

data Coterm : Inputs -> Ty -> Outputs -> Type where
  UnitMatch : Command as bs -> Coterm as Unit bs
  TensorMatch : Command (as :< a :< b) bs
    -> Coterm as (Tensor a b) bs
```

The two introduction rules can be seen as translations of the ordinary natural deduction introduction rules we saw earlier, while the two match rules are using the fact that the semantics of the input context is folded tensor product and are really just reassociating. (These are called "reversible rules"; logical rules in System L generally come in a pair of a reversible rule and a non-reversible rule.)

Now we can finally give rules to the sequence product. As the logical reification of the right hand context it behaves like the *par* operator of linear logic; unfortunately par is itself so mysterious that this is a sort of anti-metaphor. But syntactically, the rules are exactly dual to those for tensor. We call the rules "cointroduction" and "comatch", which signals both that the connective is negative and also that we are doing something counterintuitive.

```idris
data Term : Inputs -> Ty -> Outputs -> Type where
  UnitComatch : Command as bs -> Term as Unit bs
  ThenComatch : Command as (a :: b :: bs)
    -> Term as (Then a b) bs

data Coterm : Inputs -> Ty -> Outputs -> Type where
  UnitCointro : Coterm [<] Unit []
  ThenCointro : Partition as1 as2 as -> Simplex bs1 bs2 bs
    -> Coterm as1 a bs1 -> Coterm as2 b bs2
    -> Coterm as (Then a b) bs
```

Similarly to the match rules for tensor product, the comatch rules for sequence product are using the fact that the semantics of the output context is folded sequence product and are just reassociating. But the cointroduction rules are saying something genuinely new that we couldn't say in a natural deduction calculus.

## Sequence for sequents

We now have proof rules for the sequence product, but at what cost? System L is very unintuitive to program in, and is extremely unlike basically any other language I have seen. In the next post we are going to transform this calculus into something extremely familiar: an imperative programming language.

The starting point is the main result of [the Spivak-Srinivasan paper](https://arxiv.org/abs/2407.01849) is that any duoidal category whose two tensor units coincide is not just a linearly distributive category, making it a model of sequent calculus, but also an "isomix category", making it a model of sequent calculus with the mix rules.

In System L this is particularly easy to see, and it uses the fact that the same connective `Unit` has twice as many rules as usual, two introduction rules and two match rules:

```idris
mix0 : Command [<] []
mix0 = Cut [<] Z UnitIntro UnitCointro

mix : Partition as1 as2 as -> Simplex bs2 bs1 bs
  -> Command as1 bs1 -> Command as2 bs2 -> Command as bs
mix pa sx t1 t2 = Cut pa sx (UnitComatch t1) (UnitMatch t2)
```

This is going to be our semantics of imperative programming: `mix` will be our interpretation of the imperative semicolon, and `mix0` its unit, the no-op command. We'll pick this up in the next post.
