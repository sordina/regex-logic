
module Generate where

import Data
import Control.Applicative
import Control.Monad.Logic

-- Logic Functions

produceAll :: MonadLogic f => Regex -> f String
produceAll Empty          = pure ""
produceAll EOF            = pure ""
produceAll Any            = produceAll (foldr1 Alt (map Lit ['a'..'z']))
produceAll (Lit s)        = pure [s]
produceAll (Alt r1 r2)    = produceAll r1 `interleave` produceAll r2
produceAll (Concat r1 r2) = (<|>) <$> produceAll r1 <*> produceAll r2
produceAll (Kleene r)     = produceAll $ foldr (Alt . mconcat . flip replicate r) Empty [0..]

expandAll :: Regex -> [String]
expandAll = observeAll . produceAll

expandMany :: Int -> Regex -> [String]
expandMany n = observeMany n . produceAll
