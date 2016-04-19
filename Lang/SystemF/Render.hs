{-# LANGUAGE OverloadedStrings #-}
module Lang.SystemF.Render where

import Text.PrettyPrint.ANSI.Leijen

import Lang.SystemF

data Parens = Needed | Optional

ppTerm :: Term -> Doc
ppTerm = go
  where
    go t = case t of
        UnitT          -> "U"
        VarT i         -> int i
        AbsT ty t'     -> parens $ "abs#" <> ppType ty <> "." <> go t'
        TypeAbsT t'    -> parens $ "tabs." <> go t'
        AppT ta tb     -> parens $ go ta <+> go tb
        TypeAppT t' ty -> brackets $ go t' <+> ppType ty

ppType :: Type -> Doc
ppType = ppTypeParens Optional

ppTypeParens :: Parens -> Type -> Doc
ppTypeParens p ty = case ty of
    UnitTy        -> "U"
    VarTy i       -> int i
    ForallTy ty'  -> "forall." <> ppType ty'
    ArrowTy ta tb -> optParens p $ ppTypeParens Needed ta <+> "->" <+> ppTypeParens Optional tb

optParens :: Parens -> Doc -> Doc
optParens Needed   = parens
optParens Optional = id
