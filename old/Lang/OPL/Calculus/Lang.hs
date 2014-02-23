module Lang.OPL.Calculus.Lang where

-- NOTATION:
-- -- types A and B are abstract
-- --        _______________________
-- -- foo = |         _____         |
-- --       | >-A -> |     | -> A >-| -> A
-- --  A -> |/       |     |        |
-- --       |\  B -> |_____| -> B   |
-- --       | \  \-<-<-<-<-<-<-/    |
-- --       |  \  (^delay False)    |
-- --       |   \                   |
-- --       |    \>->->->->->->->->-| -> A
-- --       |_______________________|
-- --
-- -- type declaration
-- foo : forall {A B}. (A B >-> A B) -> (A >-> A A)
-- -- infered types
-- foo = tfun {A B}. fun F. wire.
--   IN x
--   loop z.
--     z' <- delay False -< z
--     y z <- F -< x z'
--   OUT y x
-- -- explicit types
-- foo = tfun {A B}. fun (F:A B >-> A B). wire (A >-> A A).
--   IN x
--   loop z.
--     z' <- delay False -< z
--     y z <- F -< x z'
--   OUT y x
-- -- unicode abreviations
-- foo = Λ {A B}. λ (F:A B >-> A B). γ (A >-> A A).
--   IN x
--   loop z.
--     z' <- delay False -< z
--     y z <- F -< x z'
--   OUT y x

-------------------- Core --------------------
type Name = String

data Prog = [Def]
data Def = Def
  { defName :: Name
  , defTerm :: Term
  }

data Exp = 
    Var_E Name
  | Function_E [(Name,Type)] Term
    TFunction_E [Name] Term
  | Wire_E Type [Name] [Statement] [Name]
  | App_T Term Term

data Type =
    Var_T Name
  | Function_T Type Type
  | TFunction_T Name Type Type
  | Wire_T [Type] [Type]

data Statement = Statement
  { statementInputs :: [Name]
  , statementAction :: Exp
  , statementOutputs :: [Name]
  }
