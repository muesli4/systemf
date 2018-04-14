{-
    This module provides abstract syntax trees for terms and types. Note that
    variables are encoded using zero based De Bruijn indices.
-}
module Lang.SystemF where

data Term
    = UnitT
    | VarT Int
    | AbsT Type Term
    | TypeAbsT Term
    | AppT Term Term
    | TypeAppT Term Type
    deriving (Eq, Show)

data Type
    = UnitTy
    | VarTy Int
    | ForallTy Type
    | ArrowTy Type Type
    deriving (Eq, Show)

-- Replace all type variables bound by the 0th forall quantifier with a given
-- substitute.
replaceTyVar :: Type -> Type -> Type
replaceTyVar = replaceTyVarWith 0

replaceTyVarWith :: Int -> Type -> Type -> Type
replaceTyVarWith initD ty targetTy = go initD targetTy
  where
    go d tty = case tty of
        UnitTy           -> UnitTy
        VarTy i          -> if i == d then ty else tty
        ForallTy bodyTy  -> ForallTy $ go (succ d) bodyTy
        ArrowTy aTy bTy  -> ArrowTy (go d aTy) (go d bTy)
