{-# LANGUAGE OverloadedStrings #-}
module Lang.SystemF.Infer.Render where

import           Lang.SystemF
import           Lang.SystemF.Infer
import           Lang.SystemF.Render
import qualified Text.PrettyPrint.ANSI.Leijen as L
import qualified Data.List.NonEmpty           as NE

ppInferError :: InferError -> L.Doc
ppInferError e = case e of
    InvalidVar v d               ->
        let detail = "index" L.<+> L.int v L.<+> "at depth" L.<+> L.int d
        in "There is no binder for the given index, namely:" <?> detail
    InvalidApp aie               ->
        ppAppInferError aie
    NoUniversalType tp           ->
        "Can't apply a type to a term without a universal type, namely:" <?> ppTyping tp
    InvalidTyVars vs d ->
        "Type annotation uses invalid type variables, namely:" <?> commaAnd (L.int <$> vs)


ppAppInferError :: AppInferError -> L.Doc
ppAppInferError e = case e of
    NoFun tp t                ->
        let detail = ppTyping tp L.<> L.comma
                     L.<+> "but expected a function with argument type" L.<+> ppType t
        in "Can't apply a term to a non-function type, namely:" <?> detail
    ParamTypeMismatch tye tya ->
        "Parameter type mismatch: " <?> d
      where
        d = "expected" L.<+> ppTypeSq tye L.<> ", actual" L.<+> ppTypeSq tya

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
