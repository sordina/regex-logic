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

prop_match_1, prop_match_2,
  prop_regex_1, prop_regex_2,
  prop_empty_1,
  prop_charParser_1, prop_charParser_2, prop_charParser_3, prop_charParser_4,
  prop_charsOrRegex_1, prop_charsOrRegex_2, prop_charsOrRegex_3, prop_charsOrRegex_4, prop_charsOrRegex_5,
  prop_regexParser_1, prop_regexParser_2, prop_regexParser_5, prop_regexParser_6, prop_regexParser_3, prop_regexParser_4
  :: Bool

prop_regex_1        = isRight $ expandMany 10 <$> parse' (regex <* eof) "a*|(b|c)*"
prop_regex_2        = isRight $ expandMany 10 <$> parse' (regex <* eof) "ab"

prop_empty_1        = isRight $ parse' regex ""

prop_charParser_1   = isRight $ parse' charParser "x"
prop_charParser_2   = isRight $ parse' charParser "\\|"
prop_charParser_3   = isLeft  $ parse' charParser "|"
prop_charParser_4   = isLeft  $ parse' (charParser <* eof) "ab"

prop_charsOrRegex_1 = EOF /= [r|asdf\|qw\\er|]
prop_charsOrRegex_2 = isLeft  $ parseRegex "asdf\\"
prop_charsOrRegex_3 = EOF /= [r|asdf\|qw\\er|]
prop_charsOrRegex_4 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_5 = isRight $ parse' (regex <* eof) "a|b"

prop_regexParser_1  = [r||]    == Empty
prop_regexParser_7  = [r|.|]   == Any
prop_regexParser_2  = [r|a|]   == Lit 'a'
prop_regexParser_5  = [r|a*|]  == Kleene (Lit 'a')
prop_regexParser_6  = [r|a|b|] == Alt (Lit 'a') (Lit 'b')
prop_regexParser_3  = [r|ab|]  == Concat (Lit 'a') (Lit 'b')
prop_regexParser_4  = [r|abc|] == Concat (Concat (Lit 'a') (Lit 'b')) (Lit 'c')

prop_match_1        = matchString [r|a|(b|c)*|] "a"
prop_match_2        = matchString [r|a|(b|c)*|] "bbbcbcbcbbbb"

