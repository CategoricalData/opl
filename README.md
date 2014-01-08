OPL
====================

Operad Programming Language (OPL) is the language of wiring.

Current Status
--------------------

* Lang.OPL.Algebra gives the algebraic representation of wirings/propegators
  - Similar to what Haskell arrows would look like with 'arr' replaced by swap/dup/assoc
  - binderless form, not planning on developing further
* Lang.OPL.AlgebraMorphism gives the algebraic representation of wiring diagrams
  - Morphisms in the whatever category Lang.OPL.Algebra lives in...
  - binderless form, also not planning on developing further
* Lang.OPL.Calculus gives a calculus with binders for wiring
  - Very close to the surface syntax of Haskell arrows
  - Notable differences with Haskell arrows are:
    + no arr
    + no desugaring to binderless form
    + a proper type system
  - Example syntax is given in the source file
* Lang.OPL.Denotation gives the set/category-theoretic formulation of wiring diagrams (morphisms)
  - Intended to be as close as possible to how David Spivak sees things, while
    coinciding with the above three languages (I claim)
  - Example syntax is given in the source file
* HelloWorld files are simple WxHaskell programs I haven't managed to get working yet...
