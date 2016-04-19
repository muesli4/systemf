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
parseTerm = runParser' termP

parseType :: String -> Either String Type
parseType = runParser' typeP

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
parensP p = symbol '(' *> p <* symbol ')'

termP :: Parser Term
termP = appP
  where
    termP' = unitP <|> varP <|> absP <|> tabsP <|> parensP appP <|> tappP
    -- term parsers
    unitP  = UnitT <$ symbol 'U'
    varP   = VarT <$> natP
    absP   = AbsT <$ lexeme "abs" <* symbol '#' <*> typeP <*> (symbol '.' *> termP)
    tabsP  = TypeAbsT <$> (lexeme "tabs" *> symbol '.' *> termP)
    appP   = chainl1 termP' (AppT <$ space)
    tappP  = TypeAppT <$ symbol '[' <*> termP' <* space <*> typeP <* symbol ']'

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
    unitP  = UnitTy <$ symbol 'U'
    varP   = VarTy <$> natP
    polyP  = ForallTy <$ lexeme "forall" <* symbol '.' <*> typeP
    arrowP = chainr1 typeP' (ArrowTy <$ lexeme "->")

natP :: Parser Int
natP = read <$> skipP (some digit)

-- | Skip all leading whitespaces from a given parser.
skipP :: Parser a -> Parser a
skipP p = many space *> p

symbol :: Char -> Parser Char
symbol = skipP . char

lexeme :: String -> Parser String
lexeme = skipP . string
