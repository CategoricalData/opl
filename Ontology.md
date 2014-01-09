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
* Operad (approximately synomymous with Symmetric Monoidal Category)
  - The mathematical system of boxes and wiring diagrams.  Contains, in addition to wiring
    diagrams, composition (boxes inside of boxes inside of boxes) and tensor (vertical juxtoposition     of boxes.

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
