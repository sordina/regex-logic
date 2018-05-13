{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module RegexQQ where

import Data
import Parse

import Language.Haskell.TH
import Language.Haskell.TH.Quote

xpr :: String -> ExpQ
xpr x =
  case parseRegex x
    of Left  e -> error $ "Failed to parse regex " ++ show e
       Right p -> [| p |]

r  :: QuasiQuoter
r = QuasiQuoter
  { quoteExp  = xpr
  , quotePat  = error "regex qq can't be used as a pattern"
  , quoteType = error "regex qq can't be used as a type"
  , quoteDec  = error "regex qq can't be used as a declaration"
  }
