{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Regex
import RegexQQ

import Data.Monoid hiding (Alt, Any)
import Text.Megaparsec
import Data.Either

-- Top Level Test

main :: IO ()
main = do
  print $ expandMany          14 $ (Alt (Kleene (Lit 'a')) (Lit 'b'))
  mapM_ putStrLn $ expandMany 4  $ (Kleene (Alt "snuggy" "buggy")) <> "bug"
  print $ [r|a|b|c*|]
  print $ [r|a|(b|c)*|]
  print $ [r|abc|]
  print $ [r| asdf |]
  print $ matchString [r|a|(b|c)*|] "a"
  print $ matchString [r|a|(b|c)*|] "bbbcbcbcbbbb"
  print $ matchString [r| asdf |] " asdf "

buggyRegex :: Regex
buggyRegex = [r|the (snuggy|buggy)*bug|]

-- Simple Props

prop_roundTrip :: String -> Bool
prop_roundTrip s = case parseRegex s
  of Left  _ -> True
     Right e -> parseRegex (pretty e) == Right e

prop_match_1, prop_match_2,
  prop_regex_1, prop_regex_2,
  prop_charParser_1, prop_charParser_2, prop_charParser_3, prop_charParser_4,
  prop_charsOrRegex_1, prop_charsOrRegex_2, prop_charsOrRegex_3, prop_charsOrRegex_4, prop_charsOrRegex_5,
  prop_regexParser_concat, prop_regexParser_concat_2, prop_regexParser_kleene, prop_regexParser_lit,
  prop_regexParser_alt, prop_regexParser_any, prop_regexParser_empty,
  prop_literal, prop_literal_2
  :: Bool

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
prop_regexParser_alt       = [r|a|b|] == Alt (Lit 'a') (Lit 'b')
prop_regexParser_concat    = [r|ab|]  == Concat (Lit 'a') (Lit 'b')
prop_regexParser_concat_2  = [r|abc|] == Concat (Concat (Lit 'a') (Lit 'b')) (Lit 'c')

prop_match_1 = matchString [r|a|(b|c)*|] "a"
prop_match_2 = matchString [r|a|(b|c)*|] "bbbcbcbcbbbb"
