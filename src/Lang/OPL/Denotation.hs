module Lang.OPL.Denotation where

data Obj = Obj
  { xin :: [Name]
  , xout :: [Name]
  -- vout :: Name -> Type
  }
  
data Hom {- X Y -} = Hom
  { f₀ :: Name {- ∈ out Y -} -> Name {- ∈ out X -} -- why not allow [in Y]
  , f₁ :: Name {- ∈ in X -} -> Name {- ∈ in Y + out X -}
  }
-- why is delayed-ness essential to this formalism?
