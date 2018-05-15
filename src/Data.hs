{-# LANGUAGE DeriveLift #-}

module Data (Regex(..), Pretty(..), pretty, specials) where

import Data.String
import Data.Monoid hiding (Alt, Any)
import Language.Haskell.TH.Syntax (Lift())

-- Datatypes and Instances

data Regex = Empty              -- The empty string
           | Any                -- Any Character
           | EOF                -- End of String
           | Lit Char           -- Character literals
           | Concat Regex Regex -- Concatenation of two regexs
           | Alt    Regex Regex -- Choice between two regexs
           | Kleene Regex       -- The Kleene star
           | Plus   Regex       -- The Kleene plus
           -- | Enum String        -- Character literals

  deriving (Eq, Lift, Show)

class PrettyShow a where
  pretty :: a -> String

instance PrettyShow a => Show (Pretty a) where
  show (Pretty r) = pretty r

newtype Pretty a = Pretty a

instance Monoid Regex where
  mempty        = Empty
  a `mappend` b = Concat a b

instance IsString Regex where fromString x = mconcat $ map Lit x

instance PrettyShow Regex where
  pretty (Lit s)
    | elem s specials            = "\\" ++ [s]
    | otherwise                  = [s]
  pretty Empty                   = ""
  pretty Any                     = "."
  pretty EOF                     = "$"
  pretty (Alt (Lit s1) (Lit s2)) = [s1] <> "|" <> [s2]
  pretty (Alt (Lit s1) r2)       = [s1] <> "|(" <> pretty r2 <> ")"
  pretty (Alt r1 (Lit s2))       = "(" <> pretty r1 <> ")|" <> [s2]
  pretty (Alt r1 r2)             = "(" <> pretty r1 <> ")|(" <> pretty r2 <> ")"
  pretty (Concat r1 r2)          = pretty r1 <> pretty r2
  pretty (Kleene (Lit s))        = [s] <> "*"
  pretty (Kleene r)              = "(" <> pretty r <> ")*"
  pretty (Plus (Lit s))          = [s] <> "+"
  pretty (Plus r)                = "(" <> pretty r <> ")+"

specials :: String
specials = "(*+|).$\\"
