module Lang.OPL.Algebra where

data Term a b where
  id :: Term a a
  (.) :: Term b c -> Term a b -> Term a c
  (⊗) :: Term a a' -> Term b b' -> Term (a,b) (a',b')
  swap :: Term (a,b) -> Term (b,a)
  rea :: Term ((a1,a2),a3) -> Term (a1,(a2,a3))
