module Lang.OPL.Calculus where

type Name = String
type Binder = [Name]

data Term = Term
  { bin :: Binder 
  , flows :: [(Binder, Term, Binder)]
  , bout :: Binder
  }

-- in Hakell arrow notation:
--    a     <- IN
--    (b,c) <- F   -< a
--    d     <- G   -< (a,b,c)
--             OUT -< (a,b,c,d)
-- where
--   bin = [a]
--   flows = [ ([a], F, [b,c])
--           , ([a,b,c], G, [d])
--           ]
--   bout = [a,b,c,d]
