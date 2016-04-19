module Lang.SystemF.Infer where

import Lang.SystemF

import qualified Data.IntMap as IM

data InferEnv = InferEnv
              { ieDepth   :: Int
              , ieVars    :: IM.IntMap Type
              }

data Typing = Typing Term Type deriving Show

data InferError = InvalidVar
                { ivVar   :: Int
                , ivDepth :: Int
                }
                | InvalidApp AppInferError
                | NoUniversalType Typing
                deriving Show

data AppInferError = NoFun Typing
                   | ParamTypeMismatch
                   { ptmExpected :: Type
                   , ptmActual   :: Type
                   }
                   deriving Show

-- | Infer the type of a closed term.
inferType :: Term -> Either InferError Type
inferType = inferTypeWith $ InferEnv 0 IM.empty

-- | Infer the type of a possibly open term, supplying an environment. This is
-- done by transforming the De Bruijn encoding, on the fly, into a global one.
inferTypeWith :: InferEnv -> Term -> Either InferError Type
inferTypeWith e@(InferEnv depth vs) t = case t of
    UnitT         -> Right UnitTy
    VarT idx      ->
        maybe (Left $ InvalidVar idx depth)
              Right
              $ IM.lookup (depth - idx - 1) vs
    AbsT ty t'    ->
        fmap (ArrowTy ty)
             $ inferTypeWith (InferEnv (succ depth) $ IM.insert depth ty vs) t'
    TypeAbsT t'   -> fmap ForallTy $ inferTypeWith e t'
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

