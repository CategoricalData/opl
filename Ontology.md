Definitions
====================

Generic Terms
--------------------

* Algebra
  - ???
* Calculus
  - Something with binders
* Combinator
  - Something without binders
* Inductive
  - A set of things defined by (finite) recursive applications of a set of
    introduction rules.
  - For example, lists of numbers are defined inductively as:

            n := *numbers*
            list l := nil
                    | cons n l

Specific Terms
--------------------

* Box
  - A box is a tuple (in,out) where 'in' and 'out' are unordered lists of types
  - Boxes look like this:

                ____
            A->|    |->C
               |    |->D
            B->|____|->E

  - Boxes are types in a sense, in that they classify machines which compute
    over the inputs, providing values over the outputs.
* Wiring Diagram
  - A mapping between boxes.
  - If wiring from box X to box Y, a wiring diagram is represented as a surjective
    function phi: (Y-Out + X-In) -> (Y-In + X-Out), such that Y-out->X-Out.
* Operad 
  - The mathematical system of self-similarity.
  - Our operad W of wiring diagrams:
    + Objects: boxes (X-In, X-Out)
    + Morphisms ϕ:X(1),X(2),...,X(n) -> Y are wiring diagrams, (in+out->out+in).
    + Composition (boxes inside of boxes inside of boxes).
  - The operad of Sets
    + Objects: Sets S
    + Morphisms ϕ:S(1),S(2),...,S(n) -> T are functions S(1)xS(2)x...xS(n) -> T.
    + Composition (usual).

* Operad functor
  - Given two operads, say L and M, we can speak of a functor P:L -> M.
    + It sends objects in L to objects in M, 
    + it sends n-ary morphisms in L to n-ary morphisms in M, and
    + it respects composition.
  - When M is Set, we call P an algebra.
  - Let L=W (wiring diagrams) and M=Sets. Then an operad functor P:W->Sets includes 
    + A mapping that sends each box X∈W to a set S∈Set.
    + A mapping that sends each wiring diagram ϕ to a function.

* Machine/Propagator
  - This is the algebra part. We have a functor Propagators: WiringDiagrams -> Set.
  - Machines describe some sort of computation in a specific domain.
  - Machines are classified by boxes (in other words, if X is a box, M is a machine to fill in that box iff M is an element of the set Propagators(X).
  - Boxes and wiring diagrams can be described and have semantics independent
    of an algebra of machines; that is, the machines are dependently typed by the operad.

Languages
--------------------

The following are described in an untyped setting for now.

[x ...] is meta-syntax for a sequence of x 'things'.

* Operad Combinators
  - Language syntax consists of the following terms e:

            e := id
               | split
               | sink
               | swap
               | assoc
               | loop e
               | e ∘ e
               | e ⊗ e
  - Terms are classified (typed) by boxes.
  - Terms are denoted by wiring diagrams which have been applied to a machine.
  - Morphisms between terms take place in the host language/logic, or a
    first-order lambda could be added for abstraction.
  - Suitable for a shallow embedding
* Operad Morphism Combinators
  - Language consists of the following terms e:

            e := id
               | split
               | sink
               | swap
               | assoc
               | loop
               | e ∘ e
               | e ⊗ e
  - Terms are classified (typed) by mappings (morphism) between boxes.
  - Terms are denoted directly by wiring diagrams.
  - No host language/logic needed to express morphisms/abstraction.
  - Suitable for a shallow embedding
* Operad Calculus
  - Language syntax consists of variables x, terms e and statements s:

            x := *variable-name*
            e := λ x. e
               | e e
               | wire [s ...]
            s := [x ...] <- e -< [x ...]
               | loop [s ...]
  - Terms are classified (typed) by both boxes and box morphisms (similar to
    lambda calculus).
  - wire [s ...] is denoted by applied wiring diagrams, and (λ x. e) is denoted
    by a wiring diagram.
  - No host language/logic needed to express morphisms/abstraction.
  - Not suitable for a shallow embedding (because of binders)
* Operad Category
  - (presented with types)
  - Other possible names:
    + Operad internal language
    + Operad core language
  - Language syntax consists of variables x, types t, boxes b, and wirings w:

            x := *variable-name*
            t := Int | Bool | ...
            b := box {in=[(x:t) ...], out=[(x:t) ...]}
            w := wiring {in=[(x, x) ...], out=[(x, x) ...]} : [b ...] -> b
  - Wirings are classified (typed) by box morphisms
  - Wirings are denoted by wiring diagrams
  - No host language/logic needed to express morphisms/abstraction.
  - Not suitable for a shallow embedding (because of binders)
  - Tensor may be problematic with naming, so for now use the operad model which allows multiple inputs.
  - (the version with tensor and a single input is called symmetric monoidal category.)
