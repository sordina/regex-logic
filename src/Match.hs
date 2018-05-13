{-# LANGUAGE QuasiQuotes #-}

module Match where

import Data
import Parse
import Generate
import Control.Arrow (first)
import Algebra.Graph hiding (Empty)
import Algebra.Graph.Export.Dot
import qualified RegexQQ as Q

type DFA  a = Graph (Node a)
type Edge a = (Node a, Node a)
type Node a = (Int, a)

main :: IO ()
main = do
  putStrLn ""
  print $ [Q.r|hello world|]
  print $ toDfa [Q.r|hello world|]
  putStrLn ""
  putStrLn $ exportViaShow $ toDfa [Q.r|hello world|]
  putStrLn ""
  print $ [Q.r|abcdefg|]
  print $ toDfa [Q.r|abcdefg|]
  putStrLn ""
  putStrLn $ exportViaShow $ toDfa [Q.r|abcdefg|]
  putStrLn ""
  print $ [Q.r|a|b|]
  print $ toDfa [Q.r|a|b|]
  putStrLn ""
  putStrLn $ exportViaShow $ toDfa [Q.r|a|b|]
  putStrLn ""
  print $ [Q.r|ab|]
  print $ toDfa [Q.r|ab|]
  putStrLn ""
  putStrLn $ exportViaShow $ toDfa [Q.r|ab|]
  putStrLn ""
  putStrLn "/a|b/ =~ ab a b c ''"
  print $ matchString [Q.r|a|b|] "ab"
  print $ matchString [Q.r|a|b|] "a"
  print $ matchString [Q.r|a|b|] "b"
  print $ matchString [Q.r|a|b|] "c"
  print $ matchString [Q.r|a|b|] ""
  putStrLn "// =~ ''"
  print $ matchString [Q.r||] ""
  putStrLn "/a/ =~ a"
  print $ matchString [Q.r|a|] "a"
  putStrLn "/ab/ =~ ab"
  print $ matchString [Q.r|ab|] "ab"
  putStrLn "/abcdefg/ =~ abcdefg"
  print $ matchString [Q.r|abcdefg|] "abcdefg"
  putStrLn "/abcdefg/ =~ abcdefgh"
  print $ matchString [Q.r|abcdefg|] "abcdefgh"

prop_matches_generated_elements :: String -> Bool
prop_matches_generated_elements s = case r
    of Left  _ -> True
       Right x -> all (matchString x) (expandMany 10 x)
  where
  r = parseRegex s

matchString :: Regex -> String -> Bool
matchString r s | null s && isEmpty i = True
                | otherwise = or $ dfaMatch g (map Just s) <$> i
  where
  i = initial g
  g = toDfa r

-- We use Maybe in order to support Any with Nothing and Chars with Just
-- We use a pair of (Int, Char) in order to allow the regex to include the
-- same character in multiple discrete places.

toDfa :: Regex -> DFA (Maybe Char)
toDfa Empty          = empty
toDfa EOF            = empty
toDfa (Lit s)        = vertex (0, Just s)
toDfa Any            = vertex (0, Nothing)
toDfa (Alt r1 r2)    = simplify $ bump succ (toDfa r1) `overlay` toDfa r2
toDfa (Concat r1 r2) = simplify $ bump succ (toDfa r1) `bridge`  toDfa r2
toDfa (Kleene r)     = simplify $ bridge rd rd where rd = toDfa r

-- Matching

dfaMatch :: (Eq a, Ord a) => DFA a -> [a] -> Node a -> Bool
dfaMatch _   []     _ = False
dfaMatch dfa [x]    n = isFinal dfa n && x == snd n
dfaMatch dfa (x:xs) n = currentMatch && restMatch
  where
  currentMatch = dfaMatch dfa [x] n
  restMatch    = or $ dfaMatch dfa xs <$> dfaTraverse dfa n x

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
