{-# LANGUAGE TupleSections, LambdaCase #-}

module Grin.ParseAST (parseGrin, parseProg, parseDef, parseExpr) where

import Data.Char
import Data.Void

import Control.Applicative (empty)
import Control.Monad (void, mzero)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C
import qualified Data.Set as Set

import Grin.Grin
import Grin.ParseBasic
import Grin.ParseTypeEnv  

-- grin syntax

def = Def <$> try (L.indentGuard sc EQ pos1 *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

expr i = L.indentGuard sc EQ i >>
  try ((\pat e b -> EBind e pat b) <$> (try (value <* op "<-") <|> pure Unit) <*> simpleExp i <*> expr i ) <|>
  ifThenElse i <|>
  simpleExp i

ifThenElse i = do
  kw "if"
  v <- value
  kw "then"
  t <- (L.indentGuard sc GT i >>= expr)
  L.indentGuard sc EQ i
  kw "else"
  e <- (L.indentGuard sc GT i >>= expr)
  return $ ECase v [ Alt (LitPat (LBool True))  t
                   , Alt (LitPat (LBool False)) e
                   ]

simpleExp i = SReturn <$ kw "pure" <*> value <|>
              ECase <$ kw "case" <*> value <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
              -- SStore <$ kw "store" <*> satisfyM nodeOrVar value <|>
              SStoreI <$ kw "store" <*> optional (brackets $ fromIntegral <$> integer) <*> satisfyM nodeOrVar value <|>
              SFetchI <$ kw "fetch" <*> var <*> optional (brackets $ fromIntegral <$> integer) <|>
              SUpdate <$ kw "update" <*> var <*> satisfyM nodeOrVar value <|>
              SBlock <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>
              SApp <$> primNameOrDefName <*> many simpleValue
  where
    nodeOrVar = \case
      ConstTagNode _ _ -> True
      VarTagNode _ _   -> True
      Var _            -> True
      _                -> False

primNameOrDefName = ('_':) <$ char '_' <*> var <|> var

alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

altPat = parens (NodePat <$> tag <*> many var) <|>
         DefaultPat <$ kw "#default" <|>
         TagPat <$> tag <|>
         LitPat <$> literal

simpleValue = Lit <$> literal <|>
              Var <$> var <|>
              Undefined <$> parens (kw "#undefined" *> op "::" *> typeAnnot)

-- #undefined can hold simple types as well as node types
value = Unit <$ op "()" <|>
        try (parens (ConstTagNode <$> tag <*> many simpleValue <|> VarTagNode <$> var <*> many simpleValue)) <|>
        ValTag <$> tag <|>
        simpleValue 

literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          LInt64 . fromIntegral <$> signedInteger <|>
          LBool <$> (True <$ kw "#True" <|> False <$ kw "#False")

satisfyM :: (a -> Bool) -> Parser a -> Parser a
satisfyM pred parser = do
  x <- parser
  if pred x
    then pure x
    else mzero


grinModule :: Parser Exp
grinModule = Program <$> many def <* sc <* eof

parseGrin :: String -> String -> Either (ParseError Char Void) Exp
parseGrin filename content = runParser grinModule filename (withoutTypeAnnots content)

parseProg :: String -> Exp
parseProg src = either (error . parseErrorPretty' src) id . parseGrin "" $ withoutTypeAnnots src

parseDef :: String -> Exp
parseDef src = either (error . parseErrorPretty' src) id . runParser def "" $ withoutTypeAnnots src

parseExpr :: String -> Exp
parseExpr src = either (error . parseErrorPretty' src) id . runParser (expr pos1) "" $ withoutTypeAnnots src


withoutTypeAnnots :: String -> String 
withoutTypeAnnots = unlines
                  . map skipIfAnnot
                  . lines
  where skipIfAnnot line
          | ('%':_) <- dropWhile isSpace line = "" 
          | otherwise = line