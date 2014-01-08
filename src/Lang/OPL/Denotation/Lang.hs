module Lang.OPL.Denotation.Lang where

-- NOTATION:
--
-- -- I is short for 'Int' and B is short for 'Bool'
-- --        _______________________
-- -- foo = |         _____         |
-- --       | >-I -> |     | -> I >-| -> I
-- --  I -> |/       |     |        |
-- --       |\  B -> |_____| -> B   |
-- --       | \  \-<-<-<-<-<-<-/    |
-- --       |  \  (^delay False)    |
-- --       |   \                   |
-- --       |    \>->->->->->->->->-| -> I
-- --       |_______________________|
-- --
-- -- the (^delay False) delays the looped wire, using False as the initial
-- -- value
--
-- operad t1 = Int Bool >-> Int Bool
-- operad t2 = Int >-> Int Int
--
-- -- direct specification
-- foo : [(W:Int) (X:Bool) >-> (Y:Int) (Z:Bool)] -> [(A:Int) >-> (B:Int) (C:Int)]
-- -- using an operad with local names
-- foo : t1 with [W X >-> Y Z] -> t2 with [A >-> B C]
-- foo =
--   W <- A
--   X <- Z delay False
--   B <- Y
--   C <- A

data Type = Int_T | Bool_T

data Operad = Operad
  { objIn :: [Type]
  , objOut :: [Type]
  }

data Tag = AIn | AOut | BIn | BOut
newtype TName (t::Tag) = TName Name
  
data Hom = Hom
  { homAIn :: [TName AIn]
  , homAOut :: [TName AOut]
  , homBIn :: [TName BIn]
  , homBOut :: [TName BOut]
  , homMap :: Either (TName BOut) (TName AIn) -> Either (TName BIn) (TName AOut)
  }
