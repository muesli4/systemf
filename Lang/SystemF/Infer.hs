module Lang.SystemF.Infer where

import           Lang.SystemF

import           Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap        as IM

data InferEnv = InferEnv
              { ieDepth     :: Int
              , ieVars      :: IM.IntMap Type
              , ieTVarDepth :: Int
              }

data Typing = Typing Term Type deriving Show

data InferError = InvalidVar
                { ivVar   :: Int
                , ivDepth :: Int
                }
                | InvalidApp AppInferError
                | NoUniversalType Typing
                | InvalidTyVars
                { itvVars  :: NE.NonEmpty Int
                , itvDepth :: Int
                }
                deriving Show

data AppInferError = NoFun Typing
                   | ParamTypeMismatch
                   { ptmExpected :: Type
                   , ptmActual   :: Type
                   }
                   deriving Show

-- | Infer the type of a closed term.
inferType :: Term -> Either InferError Type
inferType = inferTypeWith $ InferEnv 0 IM.empty 0

-- | Infer the type of a possibly open term, supplying an environment. This is
-- done by transforming the De Bruijn encoding, on the fly, into a global one.
inferTypeWith :: InferEnv -> Term -> Either InferError Type
inferTypeWith e@(InferEnv depth vs tVarDepth ) t = case t of
    UnitT         -> Right UnitTy
    VarT idx      ->
        maybe (Left $ InvalidVar idx depth)
              Right
              $ IM.lookup (depth - idx - 1) vs
    AbsT ty t'    ->
        -- Check whether the type in the type annotation uses only valid type
        -- variables.
        let e' = InferEnv (succ depth) (IM.insert depth ty vs) tVarDepth
        in case invalidTyVars tVarDepth ty of
            []     -> ArrowTy ty <$> inferTypeWith e' t'
            v : vs -> Left $ InvalidTyVars (v NE.:| vs) tVarDepth
    TypeAbsT t'   ->
        let e' = e { ieTVarDepth = succ tVarDepth }
        in ForallTy <$> inferTypeWith e' t'
    AppT tf ta    -> case (inferTypeWith e tf, inferTypeWith e ta) of
        -- Application succeeds if both can be infered and the parameter type
        -- is the same as the argument type.
        (Right (ArrowTy argTy bodyTy), Right aTy) | argTy == aTy -> Right bodyTy
                                                  | otherwise    ->
            Left $ InvalidApp $ ParamTypeMismatch argTy aTy
        (Right fTy                   , Right _)                  ->
            Left $ InvalidApp $ NoFun (Typing tf fTy)
        -- TODO combine error messages ?
        (Left e, _)                                              -> Left e
        (_, Left e)                                              -> Left e
    TypeAppT t ty -> case inferTypeWith e t of
        -- Type application succeeds if we can infer a forall quantified type.
        Right (ForallTy bodyTy) -> Right $ replaceTyVar ty bodyTy
        Right inferedTy         -> Left $ NoUniversalType (Typing t inferedTy)
        e                       -> e

-- | Check whether a type uses only valid type variables.
invalidTyVars :: Int -> Type -> [Int]
invalidTyVars tVarDepth t = case t of
    UnitTy                  -> []
    VarTy i | i < tVarDepth -> []
            | otherwise     -> [i]
    ForallTy ty             -> invalidTyVars' ty
    ArrowTy fTy aTy         -> nub $ invalidTyVars' fTy ++ invalidTyVars' aTy
  where
    invalidTyVars' = invalidTyVars tVarDepth
