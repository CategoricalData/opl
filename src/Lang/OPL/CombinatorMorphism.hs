module Lang.OPL.AlgebraMorphism where

data Term a b c d where
  Id    :: Term a b a b
  Spilt :: Term a b a (b,b)
  Sink  :: Term a b (a,c) b
  Swap  :: Term a (b,c) a (c,b)
  Assoc :: Term a ((b,c),d) a (b,(c,d))
  Loop  :: Term (a,c) (b,c) a b
  (:∘:) :: Term a b c d -> Term c d e f -> Term a b e f
  (:⊗:) :: Term a b c d -> Term a' b' c' d' -> Term (a,a') (b,b') (c,c') (d,d')
