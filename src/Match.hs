{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Match
  (exportSimple, toDFA, match, dfaMatch, DFA, Node, Edge, Rec(..))
  where

import Data
import Control.Monad.State
import Algebra.Graph.Export.Dot
import Algebra.Graph hiding (Empty)
import Data.Bool
import Data.String (fromString)

type DFA   = Graph Rec
type Node  = Rec
type Edge  = (Rec, Rec)
data Rec   = R { ini :: Bool, fin :: Bool, skp :: Bool, tok :: Char, idi :: Int } deriving (Eq, Ord, Show)

exportSimple :: DFA -> String
exportSimple = export (defaultStyle (fromString . show . idi)) { vertexAttributes = \n -> [ "label" := label n ] }
  where
  label r = bool "" "^" (ini r) ++ bool (escape $ tok r) "." (skp r) ++ bool "" "$" (fin r)
  escape x | elem x specials = "\\\\" ++ [x]
           | otherwise       = [x]

setIni :: Bool -> Rec -> Rec
setIni x r = r { ini = x }
setFin :: Bool -> Rec -> Rec
setFin x r = r { fin = x }

mkAny :: Int -> Rec
mkAny = R True True True '.'

mkLit :: Char -> Int -> Rec
mkLit = R True True False

toDFA :: Regex -> DFA
toDFA r = evalState (toDfaM r) 0

toDfaM :: Regex -> State Int DFA
toDfaM Empty          = pure empty
toDfaM EOF            = pure empty
toDfaM Any            = vertex . mkAny   <$> bump
toDfaM (Lit s)        = vertex . mkLit s <$> bump
toDfaM (Alt r1 r2)    = super            <$> toDfaM r1 <*> toDfaM r2
toDfaM (Concat r1 r2) = bridge r1 r2     <$> toDfaM r1 <*> toDfaM r2
toDfaM (Kleene r)     = loop             <$> toDfaM r
toDfaM (Plus r)       = toDfaM (Concat r (Kleene r))

vanishing :: Regex -> Bool -- Check if a regex can be completely vacuous
vanishing Empty        = True
vanishing EOF          = True
vanishing Any          = False
vanishing (Lit _)      = False
vanishing (Kleene _)   = True
vanishing (Plus r)     = vanishing (Concat r (Kleene r))
vanishing (Alt l r)    = vanishing l || vanishing r
vanishing (Concat a b) = vanishing a && vanishing b

super :: DFA -> DFA -> DFA
super a b = simplify (overlay a b)

bump :: (Enum b, MonadState b f) => f b
bump = modify succ *> get

bridge :: Regex -> Regex -> DFA -> DFA -> DFA
bridge r1 r2 a b = simplify $ overlays [trap <$> a, block <$> b, c]
  where
  c     = (trap <$> final a) `connect` (block <$> initial b)
  trap  = bool (setFin False) id (vanishing r2)
  block = bool (setIni False) id (vanishing r1)

loop :: DFA -> DFA
loop a = simplify $ overlays [a, c]
  where
  c = final a `connect` initial a

final :: DFA -> DFA
final a = vertices $ filter fin (vertexList a)

initial :: DFA -> DFA
initial a = vertices $ filter ini (vertexList a)

match :: Regex -> String -> Bool
match r s
  | null s && isEmpty i = True
  | otherwise           = or $ dfaMatch g s <$> i
  where
  i = initial g
  g = toDFA r

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
