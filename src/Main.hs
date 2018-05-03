{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.String
import Data.Monoid hiding (Alt)
import Control.Applicative hiding (many, some)
import Control.Monad.Logic
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char as C
import Data.Either
import qualified Data.List.NonEmpty as NEL
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Control.Monad.Combinators as NC

data Regex = Empty              -- The empty string
           | Lit Char           -- Character literals
           | Concat Regex Regex -- Concatenation of two regexs
           | Alt    Regex Regex -- Choice between two regexs
           | Kleene Regex       -- The Kleene star
           deriving Show

instance Monoid Regex where
  mempty        = Empty
  a `mappend` b = Concat a b

instance IsString Regex where fromString x = mconcat $ map Lit x

produceAll' :: Applicative f => (f String -> f String -> f String) -> Regex -> f String
produceAll' _x Empty          = pure ""
produceAll' _x (Lit s)        = pure [s]
produceAll'  x (Alt r1 r2)    = produceAll' x r1 `x` produceAll' x r2
produceAll'  x (Concat r1 r2) = (<|>) <$> produceAll' x r1 <*> produceAll' x r2
produceAll'  x (Kleene r)     = produceAll' x $ foldr (Alt . mconcat . flip replicate r) Empty [0..]

produceAll :: MonadLogic f => Regex -> f String
produceAll = produceAll' interleave

main :: IO ()
main = do
  print $ take                 14 $ produceAll' (<|>) (Alt (Kleene (Lit 'a')) (Lit 'b'))
  print $ observeMany          14 $ produceAll        (Alt (Kleene (Lit 'a')) (Lit 'b'))
  mapM_ putStrLn $ observeMany 4  $ produceAll $ (Kleene (Alt "snuggy" "buggy")) <> "bug"
  print $ parseRegex "xxxx"


-- Parsing and Matching

parseRegex = parse (regexParser <* eof) "<INLINE>"

match :: Regex -> String -> Bool
match = undefined

parse' :: Parsec () String Regex -> String -> Either (ParseError (Token String) ()) Regex
parse' p = parse p "<INLINE>"

regexParser :: Parsec () String Regex -- When parsing, you may parse an empty regex
regexParser = emptyParser <|> regexParserC

regexParserE :: Parsec () String Regex -- Internally, regexen should not be empty
regexParserE
     = try charParser
   <|> try kleeneParser
   <|> try altParser

regexParserC :: Parsec () String Regex -- Concat is a special final operations
regexParserC = try regexParserE <|> concatParser

specials     = "(*|)\\" :: String
escape p     = char '\\'    *> p
parens p     = NC.between (char '(') (char ')') p
emptyParser  = pure Empty  <*  eof
charParser   = Lit         <$> (noneOf specials <|> escape (C.oneOf specials))
kleeneParser = Kleene      <$> regexParserC <* char '*'
altParser    = Alt         <$> regexParserC <*> (char '|' *> regexParserC)
concatParser = Concat      <$> regexParserE <*> regexParserC
groupParser  = parens regexParserC -- Not supported yet

expr  = makeExprParser term table
term  = emptyParser <|> parens expr <|> try charParser <|> (Concat <$> try expr <*> try expr)
table = [ [ Postfix (Kleene <$ char '*') ]
        , [ InfixL  (Alt    <$ char '|') ]
        ]

-- New Tests

prop_expr_1 = isRight $ observeMany 10 . produceAll <$> parse' (expr <* eof) "a*|(b|c)*"
prop_expr_2 = isRight $ observeMany 10 . produceAll <$> parse' (expr <* eof) "ab"

-- Old Tests

prop_empty_1        = isRight $ parse' regexParser ""

prop_charParser_1   = isRight $ parse' charParser "x"
prop_charParser_2   = isRight $ parse' charParser "\\|"
prop_charParser_3   = isLeft  $ parse' charParser "|"
prop_charParser_4   = isLeft  $ parse' (charParser <* eof) "ab"

prop_charsOrRegex_1 = isRight $ parse' regexParser "asdf\\|qw\\\\er"
prop_charsOrRegex_2 = isLeft  $ parse' (regexParser <* eof) "asdf\\"
prop_charsOrRegex_3 = isRight $ parse' (regexParser <* eof) "asdf\\|qw\\\\er"
prop_charsOrRegex_4 = isLeft  $ parse' (regexParser <* eof) "asdf\\"
prop_charsOrRegex_5 = isLeft  $ parse' (regexParser <* eof) "a|b"

prop_kleeneParser_1 = isRight $ parse' (kleeneParser <* eof) "a*"
prop_kleeneParser_2 = isLeft  $ parse' (kleeneParser <* eof) "a"

prop_altParser_1    = isRight $ parse' (altParser <* eof) "a|b"
prop_altParser_2    = isLeft  $ parse' (altParser <* eof) "ab"

prop_concatParser_1 = isRight $ parse' (concatParser <* eof) "abc"
prop_concatParser_2 = isRight $ parse' (concatParser <* eof) "a*b"

prop_regexParser_1  = isRight $ parse' (regexParser <* eof) ""
prop_regexParser_2  = isRight $ parse' (regexParser <* eof) "a"
prop_regexParser_5  = isRight $ parse' (regexParser <* eof) "a*"
prop_regexParser_6  = isRight $ parse' (regexParser <* eof) "a|b"
prop_regexParser_3  = isRight $ parse' (regexParser <* eof) "ab"
prop_regexParser_4  = isRight $ parse' (regexParser <* eof) "abc"
