module Lang.OPL.Algebra where

data Term a b where
  Id    :: Term a a
  Split :: Term a (a,a)
  Sink  :: Term (a,b) a
  Swap  :: Term (a,b) (b,a)
  Assoc :: Term ((a,b),c) (a,(b,c))
  Loop  :: Term (a,c) (b,c) -> Term a b
  (:∘:) :: Term b c -> Term a b -> Term a c
  (:⊗:) :: Term a b -> Term a' b' -> Term (a,a') (b,b')

