{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Match
  where

import Data
import Parse
import Generate
import Control.Monad.State
import Algebra.Graph hiding (Empty)
import Algebra.Graph.Export.Dot
import qualified RegexQQ as Q

type DFA   = Graph Rec
type Node  = Rec
type Edge  = (Rec, Rec)
type ReDFA = DFA
data Rec   = R { ini :: Bool, fin :: Bool, skp :: Bool, tok :: Char, idi :: Int } deriving (Eq, Ord, Show)

setIni :: Bool -> Rec -> Rec
setIni x r = r { ini = x }
setFin :: Bool -> Rec -> Rec
setFin x r = r { fin = x }
setSkp :: Bool -> Rec -> Rec
setSkp x r = r { skp = x }
setTok :: Char -> Rec -> Rec
setTok x r = r { tok = x }

mkAny :: Int -> Rec
mkAny = R True True True '.'
mkLit :: Char -> Int -> Rec
mkLit = R True True False

debugRegex :: Regex -> IO ()
debugRegex a = do
  putStrLn ""
  print a
  print $ toDfa a
  putStrLn ""
  putStrLn $ exportViaShow $ toDfa a

main :: IO ()
main = do
  debugRegex [Q.r|hello world|]
  debugRegex [Q.r|abcdefg|]
  debugRegex [Q.r|aa|]
  debugRegex [Q.r|a*|]
  debugRegex [Q.r|x(abc)*|]
  debugRegex [Q.r|a|b|]
  debugRegex [Q.r|ab|]

toDfa :: Regex -> ReDFA
toDfa r = evalState (toDfaM r) 0

toDfaM :: Regex -> State Int ReDFA
toDfaM Empty          = pure empty
toDfaM EOF            = pure empty
toDfaM Any            = vertex . mkAny   <$> bump
toDfaM (Lit s)        = vertex . mkLit s <$> bump
toDfaM (Alt r1 r2)    = super            <$> toDfaM r1 <*> toDfaM r2
toDfaM (Concat r1 r2) = bridge r1 r2     <$> toDfaM r1 <*> toDfaM r2
toDfaM (Kleene r)     = loop             <$> toDfaM r

clean :: Regex -> Bool -- Check if a regex can be completely vacuous
clean Empty        = True
clean EOF          = True
clean Any          = False
clean (Lit _)      = False
clean (Kleene _)   = True
clean (Alt l r)    = clean l || clean r
clean (Concat a b) = clean a && clean b

bar :: Bool -> Node -> Node
bar True  = id
bar False = setIni False

trap :: Bool -> Node -> Node
trap True  = id
trap False = setFin False

super :: ReDFA -> ReDFA -> ReDFA
super a b = simplify (overlay a b)

bump :: (Enum b, MonadState b f) => f b
bump = modify succ *> get

bridge :: Regex -> Regex -> ReDFA -> ReDFA -> ReDFA
bridge r1 r2 a b = simplify $ overlays [trapper <$> a, barrier <$> b, c]
  where
    c       = (trapper <$> final a) `connect` (barrier <$> initial b)
    trapper = trap (clean r2)
    barrier = bar  (clean r1)

loop :: ReDFA -> ReDFA
loop a = simplify $ overlays [a, c]
  where
  c = final a `connect` initial a

final :: ReDFA -> ReDFA
final a = vertices $ filter fin (vertexList a)

initial :: ReDFA -> ReDFA
initial a = vertices $ filter ini (vertexList a)

matchString :: Regex -> String -> Bool
matchString r s | null s && isEmpty i = True
                | otherwise = or $ dfaMatch g s <$> i
  where
  i = initial g
  g = toDfa r

dfaMatch :: DFA -> String -> Node -> Bool
dfaMatch _   []     _ = False -- We have a node, so we must have something to match
dfaMatch _   [x]    n = fin n && (x == tok n || skp n)
dfaMatch dfa (x:xs) n = currentMatch && restMatch
  where
  currentMatch = skp n || x == tok n
  restMatch    = or $ dfaMatch dfa xs <$> dfaTraverse dfa n x

dfaTraverse :: DFA -> Node -> Char -> [ Node ]
dfaTraverse dfa n c
  | skp n     = map snd pairs
  | otherwise = map snd $ filter (fromSymbol c) pairs
  where
  pairs = filter ((== n) . fst) (edgeList dfa)

fromSymbol :: Char -> Edge -> Bool
fromSymbol a (n,_) = a == tok n

prop_match_1, prop_match_2, prop_match_3, prop_match_4, prop_match_5,
  prop_match_6, prop_match_7, prop_match_8, prop_match_9, prop_match_10 :: Bool

prop_match_1  = not $ matchString [Q.r|a|b|] "ab"
prop_match_2  =       matchString [Q.r|a|b|] "a"
prop_match_3  =       matchString [Q.r|a|b|] "b"
prop_match_4  = not $ matchString [Q.r|a|b|] "c"
prop_match_5  = not $ matchString [Q.r|a|b|] ""
prop_match_6  =       matchString [Q.r||] ""
prop_match_7  =       matchString [Q.r|a|] "a"
prop_match_8  =       matchString [Q.r|ab|] "ab"
prop_match_9  =       matchString [Q.r|abcdefg|] "abcdefg"
prop_match_10 = not $ matchString [Q.r|abcdefg|] "abcdefgh"

prop_matches_generated_elements :: String -> Bool
prop_matches_generated_elements s = case r
    of Left  _ -> True
       Right x -> all (matchString x) (expandMany 10 x)
  where
  r = parseRegex s
