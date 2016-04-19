module Main where

import Lang.SystemF.Parse
import Lang.SystemF.Render
import Lang.SystemF.Infer
import Lang.SystemF.Infer.Render

import Data.Bifunctor
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ ((>> putStrLn "") . process) args
  where
    process s = do
        putStrLn $ "Input: " ++ show s
        case parseTerm s of
            Left e  -> do
                putStrLn "Failed parsing term:"
                mapM_ (putStrLn . ("    " ++)) $ lines e
            Right t -> do
                putStrLn $ "Parsed term: " ++ show t
                putStrLn $ "Pretty term: " ++ show (ppTerm t)
                case inferType t of
                    Left ie  -> putStrLn $ "Failed typing: " ++ show (ppInferError ie)
                    Right ty -> do
                        putStrLn $ "Infered type: " ++ show ty
                        putStrLn $ "Output type: " ++ show (ppType ty)
