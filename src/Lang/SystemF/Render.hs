{-# LANGUAGE OverloadedStrings #-}
module Lang.SystemF.Render where

import Text.PrettyPrint.ANSI.Leijen

import Lang.SystemF

data Parens = Needed | Optional

ppTerm :: Term -> Doc
ppTerm = ppTermParens Optional

ppTermParens :: Parens -> Term -> Doc
ppTermParens p t = case t of
    UnitT          -> "U"
    VarT i         -> int i
    AbsT ty t'     -> optParens p $ "abs#" <> ppType ty <> "." <> ppTerm t'
    TypeAbsT t'    -> optParens p $ "tabs." <> ppTerm t'
    AppT ta tb     -> optParens p $ ppTermParens Needed ta <+> ppTerm tb
    TypeAppT t' ty -> brackets $ ppTermParens Needed t' <+> ppType ty

ppType :: Type -> Doc
ppType = ppTypeParens Optional

ppTypeParens :: Parens -> Type -> Doc
ppTypeParens p ty = case ty of
    UnitTy        -> "U"
    VarTy i       -> int i
    ForallTy ty'  -> optParens p $ "forall." <> ppType ty'
    ArrowTy ta tb -> optParens p $ ppTypeParens Needed ta <+> "->" <+> ppType tb

optParens :: Parens -> Doc -> Doc
optParens Needed   = parens
optParens Optional = id
