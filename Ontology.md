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

* Machine/Propagator
  - Machines describe some sort of computation in a specific domain.
  - Machines are classified by boxes.
  - Boxes and wiring diagrams can be described and have semantics independent
    of an algebra of machines.
* Box
  - A box is a tuple (in,out) where 'in' and 'out' are lists of types
  - Boxes look like this:
            ____
        A->|    |->C
           |    |
        B->|____|->D

  - Boxes are types in a sense, in that they classify machines which compute
    over the inputs, providing values over the outputs.
* Wiring Diagram
  - A mapping between boxes.
  - If wiring from box A to box B, a wiring diagram is represented as a mapping
    from (B-Out + A-In) -> (B-In + A-Out).
* Operad
  - A system of boxes and wiring diagrams.  Contains, in addition to wiring
    diagrams, composition and tensor.

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
           | loop
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
        w := wiring {in=[(x:t, x:t) ...], out=[(x:t , x:t) ...]} : b -> b
  - Wirings are classified (typed) by box morphisms
  - Wirings are denoted by wiring diagrams
  - No host language/logic needed to express morphisms/abstraction.
  - Not suitable for a shallow embedding (because of binders)
