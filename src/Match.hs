
module Match where

import Data
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either

-- Matching

matchString :: Regex -> String -> Bool
matchString r s = isRight $ parse (matcher r) "<MATCHER>" s

matcher :: Regex -> Parsec () String ()
matcher Empty          = return ()
matcher EOF            = eof
matcher Any            = () <$ anyChar
matcher (Alt r1 r2)    = try (matcher r1) <|> matcher r2
matcher (Lit s)        = () <$ char s
matcher (Concat r1 r2) = matcher r1  *> matcher r2
matcher (Kleene r)     = eof <|> (try (matcher r) *> matcher (Kleene r))
