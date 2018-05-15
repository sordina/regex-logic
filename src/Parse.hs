{-# LANGUAGE TypeFamilies #-}

module Parse ( literal, regex, parseRegex, parse', charParser ) where

import Data

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char as C

-- Parsing

parseRegex :: String -> Either (ParseError Char ()) Regex
parseRegex = parse' (regex <* eof)

literal :: String -> Regex
literal []      = Empty
literal s@(_:_) = foldr1 Concat $ map Lit s

parse' :: Parsec () String Regex -> String -> Either (ParseError (Token String) ()) Regex
parse' p = parse p "<PARSER>"

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
term = parens regex <|> (Any <$ char '.') <|> charParser

table :: (Token s ~ Char, MonadParsec e s m) => [[Operator m Regex]]
table = [ [ Postfix (Kleene <$ char '*') ]
        , [ Postfix (Plus <$ char '+') ]
        , [ InfixL  (pure (\x y -> Concat x y)) ]
        , [ InfixL  (Alt <$ char '|') ]
        ]
