module Lang.SystemF.Unify where

import Lang.SystemF

data Eqt = Eqt Type Type deriving Show

-- | Determines type equalities, for to types to be matched.
unify :: Type -> Type -> Maybe [Eqt]
unify tx ty = go [Eqt tx ty]
  where
    go es = case es of
        []      -> Just []
        e : es' -> case e of
            Eqt UnitTy UnitTy                       -> go es'
            Eqt (VarTy i) (VarTy j)                 -> Nothing -- TODO
            Eqt (ForallTy ta) (ForallTy tb)         -> go (Eqt ta tb : es') -- TODO
            Eqt (ArrowTy ta1 ta2) (ArrowTy tb1 tb2) -> go (Eqt ta1 tb1 : Eqt ta2 tb2 : es')
            _                                       -> Nothing

-- data Type = UnitTy
--           | VarTy Int
--           | ForallTy Type
--           | ArrowTy Type Type
