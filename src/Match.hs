{-# LANGUAGE QuasiQuotes #-}

module Match where

import Data
import Control.Arrow (first)
import Algebra.Graph hiding (Empty)
import Algebra.Graph.Export.Dot
import qualified RegexQQ as Q

type DFA  a = Graph (Node a)
type Edge a = (Node a, Node a)
type Node a = (Int, a)

main :: IO ()
main = putStrLn $ exportViaShow $ toDfa [Q.r|hello world|]

matchString :: Regex -> String -> Bool
matchString r s = or $ dfaMatch g (map Just s) <$> initial g where g = toDfa r

-- We use Maybe in order to support Any with Nothing and Chars with Just
toDfa :: Regex -> DFA (Maybe Char)
toDfa Empty          = empty
toDfa EOF            = empty
toDfa (Lit s)        = vertex (0, Just s)
toDfa Any            = vertex (0, Nothing)
toDfa (Alt r1 r2)    = toDfa r1 `overlay` bump ((*2) . succ) (toDfa r2)
toDfa (Concat r1 r2) = toDfa r1 `bridge`  bump ((*2) . succ) (toDfa r2)
toDfa (Kleene r)     = bridge rd rd where rd = toDfa r

-- Matching

dfaMatch :: (Eq a, Ord a) => DFA a -> [a] -> Node a -> Bool
dfaMatch dfa []     n = isFinal dfa n
dfaMatch dfa [x]    n = isFinal dfa n && x == snd n
dfaMatch dfa (x:xs) n = or $ dfaMatch dfa xs <$> dfaTraverse dfa n x

dfaTraverse :: (Eq a, Ord a) => DFA a -> Node a -> a -> [ Node a ]
dfaTraverse dfa state a = map snd $ filter (fromSymbol a & elem state) (edgeList dfa)
  where
  (f & g) x = f x && g x

fromSymbol :: Eq a =>  a -> Edge a -> Bool
fromSymbol a ((_,x),_) = a == x

isFinal :: (Eq a, Ord a) => DFA a -> Node a -> Bool
isFinal d n | hasVertex n (final d) = True
            | otherwise             = False

-- Graph Stuff

bridge :: (Eq a, Ord a) => Graph a -> Graph a -> Graph a
bridge a b = overlays [a,b,c]
  where
  c = final a `connect` initial b

initial :: Ord a => Graph a -> Graph a
initial a = vertices (vl // el)
  where
  vl = vertexList a
  el = map snd $ edgeList a

final :: Ord a => Graph a -> Graph a
final a = vertices (vl // el)
  where
  vl = vertexList a
  el = map fst $ edgeList a

bump :: (Int -> Int) -> DFA a -> DFA a
bump f d = first f <$> d

(//) :: Eq a => [a] -> [a] -> [a]
xs // ys = filter (not . flip elem ys) xs
