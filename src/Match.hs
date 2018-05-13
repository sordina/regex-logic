{-# LANGUAGE QuasiQuotes #-}

module Match where

import Data
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either
import Data.Maybe
import Control.Monad

-- Matching

matchString :: Regex -> String -> Bool
matchString r s = isRight $ parse (matcher r) "<MATCHER>" s

matcher :: Regex -> Parsec () String ()
matcher Empty          = return ()
matcher EOF            = eof
matcher Any            = () <$ anyChar
matcher (Alt r1 r2)    = try (matcher r1) <|> try (matcher r2)
matcher (Lit s)        = () <$ char s
matcher (Concat r1 r2) = try (matcher r1) *> try (matcher r2)
matcher (Kleene r)     = eof <|> try (matcher r *> matcher (Kleene r))

-- http://johnbender.us/2009/11/08/using-a-haskell-dfa-type-to-match-strings/

type Dfa  a  = [ Node a ]
data Edge a  = Edge      { action :: (a -> Bool), edge_identifier :: Int }
data Node a  = Node      { identifier :: Int, edges :: [Edge a] }
             | FinalNode { identifier :: Int, edges :: [Edge a] }

toDfa :: Int -> Regex -> (Node Char, Dfa Char)
toDfa n Empty   = (x, [x]) where x = FinalNode (succ n) []
toDfa n EOF     = (x, [x]) where x = FinalNode (succ n) []
toDfa n (Lit s) = (x, [x,y])
  where
    x = Node (n + 1) [ Edge (== s) (identifier y) ]
    y = FinalNode (n + 2) []

toDfa n Any = (x, [x,y])
  where
    x = Node (n + 1) [ Edge (const True) (identifier y) ]
    y = FinalNode (n + 2) []

toDfa n (Alt r1 r2) = (x, [x] ++ r1d ++ r2d)
  where
    x        = Node (n + 1) [ Edge (const True) (identifier n1), Edge (const True) (identifier  n2) ]
    (n1,r1d) = toDfa 0 r1
    (n2,r2d) = toDfa 0 r2


dfaMatch :: Node a -> Dfa a -> [a] -> Bool
dfaMatch current_state _   []     = is_final current_state
dfaMatch current_state dfa (x:xs) = fromMaybe False $ (\n -> dfaMatch n dfa xs) <$> next_state
              where next_state = dfaTraverse dfa current_state x

dfaTraverse :: Dfa a -> Node a -> a -> Maybe (Node a)
dfaTraverse dfa state = (find_state dfa) <=< (next_id $ edges state)

is_final (FinalNode x y) = True
is_final (Node x y) = False

find_state :: Dfa a -> Int -> Maybe (Node a)
find_state []       state_id = Nothing
find_state (st:sts) state_id | identifier st == state_id = Just st
                             | otherwise = find_state sts state_id

next_id :: [Edge a] -> a -> Maybe Int
next_id (e:es) input | (action e) input = Just (edge_identifier e)
                     | otherwise = next_id es input

next_id []     input = Nothing


-- Just in here to speed up debug cycle
--
-- prop_match_6 = matchString [r|a*a|] "a"
-- Concat (Kleene (Lit 'a')) (Lit 'a')

{-

From http://www.cs.cornell.edu/courses/cs312/2006fa/recitations/rec26.html

type state = int   (* Note: 0 is the start state *)
  datatype dest =
    State of state (* Edge destination is another state *)
  | Error          (* if no next state *)

  type DFA = {table: dest Array2.array, accept: bool Array.array}

  (* Check whether the DFA "pattern" accepts "text" *)
  fun search({table, accept}: DFA, text: string): bool = let
    (* Give the next state of the DFA, assuming that it is in
     * state "state" and the next character is text[pos]. *)
    fun next_state(pos: int, state: int): dest =
      if pos = String.size(text) then EOF
      else let val char = Char.ord(String.sub(text, pos)) in
        Array2.sub(table, state, char)
      end
    (* Walk the DFA down the string "text" from position "pos",
     * returning whether it accepts the rest of the string. *)
    fun search1(pos: int, state: int): bool =
      if pos = String.size(text) then Array.sub(accept,state)
      else
        case next_state(pos, state) of
             Error => false
           | State(s) => search1(pos+1, s)
  in
    search1(0,0)
  end
-}
