{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.String
import Data.Monoid hiding (Alt)
import Control.Applicative
import Control.Monad.Logic
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char as C
import Data.Either

-- Datatypes and Instances

data Regex = Empty              -- The empty string
           | Lit Char           -- Character literals
           | Concat Regex Regex -- Concatenation of two regexs
           | Alt    Regex Regex -- Choice between two regexs
           | Kleene Regex       -- The Kleene star

instance Show Regex where
  show r = "/" <> pretty r <> "/"

instance Monoid Regex where
  mempty        = Empty
  a `mappend` b = Concat a b

instance IsString Regex where fromString x = mconcat $ map Lit x

-- Top Level Test

main :: IO ()
main = do
  print $ observeMany          14 $ produceAll (Alt (Kleene (Lit 'a')) (Lit 'b'))
  mapM_ putStrLn $ observeMany 4  $ produceAll $ (Kleene (Alt "snuggy" "buggy")) <> "bug"
  print $ parseRegex "a|b|c*"
  print $ parseRegex "a|(b|c)*"
  print $ parseRegex "abc"
  print $ flip matchString "a" <$> parseRegex "a|(b|c)*"
  print $ flip matchString "bbbcbcbcbbbb" <$> parseRegex "a|(b|c)*"

buggyRegex :: Either (ParseError Char ()) Regex
buggyRegex = parseRegex "the (snuggy|buggy)*bug"

-- Print Functions

pretty :: Regex -> String
pretty (Lit s)
  | elem s specials            = "\\" ++ [s]
  | otherwise                  = [s]
pretty Empty                   = ""
pretty (Alt (Lit s1) (Lit s2)) = [s1] <> "|" <> [s2]
pretty (Alt (Lit s1) r2)       = [s1] <> "|(" <> pretty r2 <> ")"
pretty (Alt r1 (Lit s2))       = "(" <> pretty r1 <> ")|" <> [s2]
pretty (Alt r1 r2)             = "(" <> pretty r1 <> ")|(" <> pretty r2 <> ")"
pretty (Concat r1 r2)          = pretty r1 <> pretty r2
pretty (Kleene (Lit s))        = [s] <> "*"
pretty (Kleene r)              = "(" <> pretty r <> ")*"

-- Logic Functions

produceAll :: MonadLogic f => Regex -> f String
produceAll Empty          = pure ""
produceAll (Lit s)        = pure [s]
produceAll (Alt r1 r2)    = produceAll r1 `interleave` produceAll r2
produceAll (Concat r1 r2) = (<|>) <$> produceAll r1 <*> produceAll r2
produceAll (Kleene r)     = produceAll $ foldr (Alt . mconcat . flip replicate r) Empty [0..]

-- Matching

matchString :: Regex -> String -> Bool
matchString r s = isRight $ parse (matcher r) "<MATCHER>" s

matcher :: Regex -> Parsec () String ()
matcher Empty          = eof
matcher (Alt r1 r2)    = try (matcher r1) <|> matcher r2
matcher (Lit s)        = char s *> pure ()
matcher (Concat r1 r2) = matcher r1  *> matcher r2
matcher (Kleene r)     = eof <|> (try (matcher r) *> matcher (Kleene r))

-- Parsing

parseRegex :: String -> Either (ParseError Char ()) Regex
parseRegex = parse' (regex <* eof)

parse' :: Parsec () String Regex -> String -> Either (ParseError (Token String) ()) Regex
parse' p = parse p "<PARSER>"

specials :: String
specials = "(*|)\\"

escape :: (Token s ~ Char, MonadParsec e s f) => f b -> f b
escape p = char '\\' *> p

parens :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
parens p = between (char '(') (char ')') p

emptyEOF :: MonadParsec e s f => f Regex
emptyEOF = Empty <$ eof

charParser :: (Token s ~ Char, MonadParsec e s f) => f Regex
charParser = Lit <$> (noneOf specials <|> escape (C.oneOf specials))

regex :: (Token s ~ Char, MonadParsec e s m) => m Regex
regex = emptyEOF <|> makeExprParser term table

term :: (Token s ~ Char, MonadParsec e s m) => m Regex
term = parens regex <|> charParser

table :: (Token s ~ Char, MonadParsec e s m) => [[Operator m Regex]]
table = [ [ Postfix (Kleene <$ char '*') ]
        , [ InfixL  (pure (\x y -> Concat x y)) ]
        , [ InfixL  (Alt <$ char '|') ]
        ]

-- Simple Props

prop_match_1, prop_match_2,
  prop_regex_1, prop_regex_2,
  prop_empty_1,
  prop_charParser_1, prop_charParser_2, prop_charParser_3, prop_charParser_4,
  prop_charsOrRegex_1, prop_charsOrRegex_2, prop_charsOrRegex_3, prop_charsOrRegex_4, prop_charsOrRegex_5,
  prop_regexParser_1, prop_regexParser_2, prop_regexParser_5, prop_regexParser_6, prop_regexParser_3, prop_regexParser_4
  :: Bool

prop_regex_1        = isRight $ observeMany 10 . produceAll <$> parse' (regex <* eof) "a*|(b|c)*"
prop_regex_2        = isRight $ observeMany 10 . produceAll <$> parse' (regex <* eof) "ab"

prop_empty_1        = isRight $ parse' regex ""

prop_charParser_1   = isRight $ parse' charParser "x"
prop_charParser_2   = isRight $ parse' charParser "\\|"
prop_charParser_3   = isLeft  $ parse' charParser "|"
prop_charParser_4   = isLeft  $ parse' (charParser <* eof) "ab"

prop_charsOrRegex_1 = isRight $ parse' regex "asdf\\|qw\\\\er"
prop_charsOrRegex_2 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_3 = isRight $ parse' (regex <* eof) "asdf\\|qw\\\\er"
prop_charsOrRegex_4 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_5 = isRight $ parse' (regex <* eof) "a|b"

prop_regexParser_1  = isRight $ parse' (regex <* eof) ""
prop_regexParser_2  = isRight $ parse' (regex <* eof) "a"
prop_regexParser_5  = isRight $ parse' (regex <* eof) "a*"
prop_regexParser_6  = isRight $ parse' (regex <* eof) "a|b"
prop_regexParser_3  = isRight $ parse' (regex <* eof) "ab"
prop_regexParser_4  = isRight $ parse' (regex <* eof) "abc"

prop_match_1        = (== Right True) $ flip matchString "a" <$> parseRegex "a|(b|c)*"
prop_match_2        = (== Right True) $ flip matchString "bbbcbcbcbbbb" <$> parseRegex "a|(b|c)*"
