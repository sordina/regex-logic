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
  print $ matchString [r|a|(b|c)*|] "a"
  print $ matchString [r|a|(b|c)*|] "bbbcbcbcbbbb"
  print $ [r| asdf |]
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

prop_charsOrRegex_1 = isRight $ parse' regex "asdf\\|qw\\\\er"
prop_charsOrRegex_2 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_3 = isRight $ parse' (regex <* eof) "asdf\\|qw\\\\er"
prop_charsOrRegex_4 = isLeft  $ parse' (regex <* eof) "asdf\\"
prop_charsOrRegex_5 = isRight $ parse' (regex <* eof) "a|b"

prop_regexParser_1  = isRight $ parse' (regex <* eof) ""
prop_regexParser_2  = isRight $ parse' (regex <* eof) "a"
prop_regexParser_5  = isRight $ parse' (regex <* eof) "a*"
prop_regexParser_6  = isRight $ parse' (regex <* eof) "a|b"
prop_regexParser_3  = isRight $ parse' (regex <* eof) "ab"
prop_regexParser_4  = isRight $ parse' (regex <* eof) "abc"

prop_match_1        = (== Right True) $ flip matchString "a" <$> parseRegex "a|(b|c)*"
prop_match_2        = (== Right True) $ flip matchString "bbbcbcbcbbbb" <$> parseRegex "a|(b|c)*"

