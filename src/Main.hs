{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Regex
import RegexQQ

import System.Exit
import System.Environment
import Data.Monoid hiding (Alt, Any)
import Text.Megaparsec hiding (match)
import Data.Either
import Algebra.Graph.Export.Dot
import Data.Maybe
import Data.List (intercalate)

-- Top Level Test

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go ("dfa"      : xs) = runDFA xs
go ("valid"    : xs) = runValid xs
go ("match"    : xs) = runMatch xs
go ("generate" : xs) = runGenerate xs
go _                 = help >> exitFailure

help :: IO ()
help = putStrLn "Usage: regex-logic (dfa | valid | match | generate) REGEX ..."

runDFA :: [String] -> IO ()
runDFA []   = helpMatch
runDFA (xs) = mapM_ (printDFA . fmap (exportViaShow . toDFA) . parseRegex) xs

printDFA :: Show a => Either a String -> IO ()
printDFA (Left x) = print x
printDFA (Right x) = putStrLn x

helpDFA :: IO ()
helpDFA = putStrLn "Usage: regex-logic dfa REGEX*"

runValid :: [String] -> IO ()
runValid []   = helpMatch
runValid (xs) = mapM_ (print . parseRegex) xs

helpValid :: IO ()
helpValid = putStrLn "Usage: regex-logic valid REGEX*"

runMatch :: [String] -> IO ()
runMatch []      = helpMatch
runMatch (x: xs) = case (parseRegex x, xs)
  of (Left  e,  _) -> error (show e)
     (Right e, []) -> interact (unlines . mapMaybe (grep e) . lines)
     (Right e, ys) -> print $ map (match e) ys

helpMatch :: IO ()
helpMatch = putStrLn "Usage: regex-logic match REGEX STRING*"

grep :: Regex -> String -> Maybe String
grep e l | match e l = Just l
         | otherwise = Nothing

runGenerate :: [String] -> IO ()
runGenerate [] = helpGenerate >> exitFailure
runGenerate rs = case parseRegex (intercalate "|" $ map parenthesise rs)
  of Left  e -> error (show e)
     Right e -> mapM_ putStrLn (expandAll e)

helpGenerate :: IO ()
helpGenerate = putStrLn "Usage: regex-logic generate REGEX*"

parenthesise :: String -> String
parenthesise s = "(" ++ s ++ ")"

bla :: IO ()
bla = do
  print $ expandMany          14 $ (Alt (Kleene (Lit 'a')) (Lit 'b'))
  mapM_ putStrLn $ expandMany 4  $ (Kleene (Alt "snuggy" "buggy")) <> "bug"
  print $ [r|a|b|c*|]
  print $ [r|a|(b|c)*|]
  print $ [r|abc|]
  print $ [r| asdf |]
  print $ match [r|a|(b|c)*|] "a"
  print $ match [r|a|(b|c)*|] "bbbcbcbcbbbb"
  print $ match [r| asdf |] " asdf "

  debugRegex [r|hello world|]
  debugRegex [r|abcdefg|]
  debugRegex [r|aa|]
  debugRegex [r|a*|]
  debugRegex [r|x(abc)*|]
  debugRegex [r|a|b|]
  debugRegex [r|ab|]

debugRegex :: Regex -> IO ()
debugRegex a = do
  putStrLn ""
  print a
  print $ toDFA a
  putStrLn ""
  putStrLn $ exportViaShow $ toDFA a

-- Simple Props

prop_genmatch, prop_roundTrip :: String -> Bool
prop_roundTrip s = case parseRegex s
  of Left  _ -> True
     Right e -> parseRegex (pretty e) == Right e

prop_match_1, prop_match_2, prop_match_3, prop_match_4, prop_match_5,
  prop_regex_1, prop_regex_2, prop_regex_3,
  prop_charParser_1, prop_charParser_2, prop_charParser_3, prop_charParser_4,
  prop_charsOrRegex_1, prop_charsOrRegex_2, prop_charsOrRegex_3, prop_charsOrRegex_4, prop_charsOrRegex_5,
  prop_regexParser_concat, prop_regexParser_concat_2, prop_regexParser_kleene, prop_regexParser_lit,
  prop_regexParser_alt, prop_regexParser_any, prop_regexParser_empty,
  prop_literal, prop_literal_2, prop_regexParser_kleene_2, prop_match_6
  :: Bool

prop_genmatch s = case (\g -> all (match g) (expandMany 10 g)) <$> parseRegex s
  of Left  _ -> True
     Right b -> b

prop_match_1 = match [r|a|(b|c)*|] "a"
prop_match_2 = match [r|a|(b|c)*|] "bbbcbcbcbbbb"
prop_match_3 = match [r|a(b|c)*|] "abbbcbcbcbbbb"
prop_match_4 = match [r|a(b|c)*|] "a"
prop_match_5 = match [r|the (snuggy|buggy)*bug|] "the snuggybug"
prop_match_6 = match [r|a*a|] "a"

prop_literal   = literal "ab" == Concat (Lit 'a') (Lit 'b')
prop_literal_2 = literal ".|" == Concat (Lit '.') (Lit '|')

prop_regex_1 = (==10) $ length $ expandMany 10 [r|a*|(b|c)*|]
prop_regex_2 = (==2) $ length $ expandAll [r|a|b|]
prop_regex_3 = (==1) $ length $ expandAll [r|ab|]

prop_charParser_1 = isRight $ parse' charParser "x"
prop_charParser_2 = isRight $ parse' charParser "\\|"
prop_charParser_3 = isLeft  $ parse' charParser "|"
prop_charParser_4 = isLeft  $ parse' (charParser <* eof) "ab"

prop_charsOrRegex_1 = EOF /= [r|asdf\|qw\\er|]
prop_charsOrRegex_2 = isLeft  $ parseRegex "asdf\\"
prop_charsOrRegex_3 = EOF /= [r|asdf\|qw\\er|]
prop_charsOrRegex_4 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_5 = isRight $ parse' (regex <* eof) "a|b"

prop_regexParser_empty     = [r||]    == Empty
prop_regexParser_any       = [r|.|]   == Any
prop_regexParser_lit       = [r|a|]   == Lit 'a'
prop_regexParser_kleene    = [r|a*|]  == Kleene (Lit 'a')
prop_regexParser_kleene_2  = [r|a*a|] == Concat (Kleene (Lit 'a')) (Lit 'a')
prop_regexParser_alt       = [r|a|b|] == Alt (Lit 'a') (Lit 'b')
prop_regexParser_concat    = [r|ab|]  == Concat (Lit 'a') (Lit 'b')
prop_regexParser_concat_2  = [r|abc|] == Concat (Concat (Lit 'a') (Lit 'b')) (Lit 'c')

prop_match_a1, prop_match_a2, prop_match_a3, prop_match_a4, prop_match_a5,
  prop_match_a6, prop_match_a7, prop_match_a8, prop_match_a9, prop_match_a10 :: Bool

prop_match_a1  = not $ match [r|a|b|] "ab"
prop_match_a2  =       match [r|a|b|] "a"
prop_match_a3  =       match [r|a|b|] "b"
prop_match_a4  = not $ match [r|a|b|] "c"
prop_match_a5  = not $ match [r|a|b|] ""
prop_match_a6  =       match [r||] ""
prop_match_a7  =       match [r|a|] "a"
prop_match_a8  =       match [r|ab|] "ab"
prop_match_a9  =       match [r|abcdefg|] "abcdefg"
prop_match_a10 = not $ match [r|abcdefg|] "abcdefgh"

prop_matches_generated_elements :: String -> Bool
prop_matches_generated_elements s = case parseRegex s
  of Left  _ -> True
     Right x -> all (match x) (expandMany 10 x)
