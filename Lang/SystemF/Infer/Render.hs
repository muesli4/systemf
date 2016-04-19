{-# LANGUAGE OverloadedStrings #-}
module Lang.SystemF.Infer.Render where

import           Lang.SystemF
import           Lang.SystemF.Infer
import           Lang.SystemF.Render
import qualified Text.PrettyPrint.ANSI.Leijen as L
import qualified Data.List.NonEmpty           as NE

ppInferError :: InferError -> L.Doc
ppInferError e = case e of
    InvalidVar _ _               ->
        "There is no binder for the given index."
    InvalidApp aie               ->
        ppAppInferError aie
    NoUniversalType tp           ->
        "Can't apply a type to a term without a universal type, namely:" <?> ppTyping tp
    InvalidTyVars vs d ->
        "Type annotation uses invalid type variables, namely:" <?> commaAnd (L.int <$> vs)


ppAppInferError :: AppInferError -> L.Doc
ppAppInferError e = case e of
    NoFun tp -> "Can't apply a term to a non-function type, namely:" <?> ppTyping tp
    ParamTypeMismatch tye tya -> "Parameter type mismatch: " <?> "expected" L.<+> ppTypeSq tye L.<> ", actual" L.<+> ppTypeSq tya

ppTypeSq :: Type -> L.Doc
ppTypeSq = L.squotes . ppType

ppTyping :: Typing -> L.Doc
ppTyping (Typing t ty) = L.squotes (ppTerm t) L.<+> "typed as" L.<+> ppTypeSq ty

-- | Add some more detail indented on a new line.
(<?>) :: L.Doc -> L.Doc -> L.Doc
title <?> detail = title L.<$> L.indent 4 detail

commaAnd :: NE.NonEmpty L.Doc -> L.Doc
commaAnd (d NE.:| ds) = case ds of
    [] -> d
    _  -> comma ds L.<+> "and" L.<+> d

comma :: [L.Doc] -> L.Doc
comma = L.cat . L.punctuate ", "
