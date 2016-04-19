module Lang.SystemF.Parse
    ( parseTerm
    , parseType
    ) where

import Lang.SystemF

import Data.Functor
import Data.Bifunctor
import Text.Parsec

import Control.Applicative (some)

type Parser = Parsec String ()

runParser' :: Parser a -> String -> Either String a
runParser' p = first show . runParser p () srcFileName 
  where
    srcFileName :: String
    srcFileName = ""

parseTerm :: String -> Either String Term
parseTerm = runParser' (spaces *> termP)

parseType :: String -> Either String Type
parseType = runParser' (spaces *> typeP)

{-
    Example terms:
        monomorphic identity function:
            abs#U.0

        monomorphic constant function:
            abs#U.abs#U.1

        application of terms:
            (abs#U.0) abs#U.0

        polymorphic identity function:
            tabs.abs#0.0

        type application:
            [(tabs.abs#0.0) U]

        higher-kinded parameter:
            abs#U -> U -> U.0 U U

        polymorphic parameter:
            abs#(forall.0 -> U).abs#forall.0.1 0

    Grammar:
        term  ::= unit | var | abs | tabs | app | tapp
        unit  ::= 'U'
        var   ::= nat
        abs   ::= 'abs' '#' type '.' term
        tabs  ::= 'tabs' '.' term
        app   ::= term+
        tapp  ::= '[' term ' ' type ']'
-}

parensP :: Parser a -> Parser a
parensP p = char '(' *> p <* spaces <* char ')'

termP :: Parser Term
termP = appP
  where
    termP' = unitP <|> varP <|> absP <|> tabsP <|> parensP appP <|> tappP
    -- term parsers
    unitP  = UnitT <$ char 'U'
    varP   = VarT <$> natP
    absP   = AbsT <$ string "abs" <* spaces <* char '#' <* spaces <*> typeP <*> (char '.' *> spaces *> termP)
    tabsP  = TypeAbsT <$> (string "tabs" *> spaces *> char '.' *> spaces *> termP)
    appP   = chainl1 (termP' <* spaces) (pure AppT)
    tappP  = TypeAppT <$ char '[' <*> termP' <* space <*> typeP <* char ']'

{-

    Grammar:
        type  ::= unit | var | poly | arrow
        unit  ::= 'U'
        var   ::= nat
        poly  ::= 'forall' '.' type
        arrow ::= type ('->' type)+

-}
typeP :: Parser Type
typeP = arrowP
  where
    typeP' = unitP <|> varP <|> polyP <|> parensP arrowP
    -- type parsers
    unitP  = UnitTy <$ char 'U' <* spaces
    varP   = VarTy <$> natP <* spaces
    polyP  = ForallTy <$ string "forall" <* char '.' <*> typeP
    arrowP = chainr1 typeP' (ArrowTy <$ string "->" <* spaces)

natP :: Parser Int
natP = read <$> some digit
