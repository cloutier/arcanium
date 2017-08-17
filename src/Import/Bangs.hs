{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Import.Bangs where

import Import
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix

findBangs :: String -> Maybe String
findBangs query
  | matches "g" = result "g" "https://google.com/search?q="
  | matches "google" = result "google" "https://google.com/search?q="
  | matches "b" = result "b" "https://www.bing.com/search?q="
  | matches "bing" = result "bing" "https://www.bing.com/search?q="
  | matches "ddg" = result "ddg" "https://www.duckduckgo.com/?q="
  | matches "wen" = result "wen" "https://en.wikipedia.org/w/index.php?search="
  | matches "wfr" = result "wfr" "https://fr.wikipedia.org/w/index.php?search="
  | matches "aca" = result "aca" "https://www.amazon.ca/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
  | matches "wikidata" = result "wikidata" "https://www.wikidata.org/w/index.php?search="
  | matches "wdt" = result "wdt" "https://www.wikidata.org/w/index.php?search="
  | matches "gh" = result "gh" "https://github.com/search?utf8=%E2%9C%93&q="
  | matches "tpb" = result "tpb" "https://thepiratebay.org/search/"
  | matches "wa" = result "wa" "https://www.wolframalpha.com/input/?i="
  | otherwise = Nothing
  where
    matches :: String -> Bool
    matches x = query =~ (bangRegex x) :: Bool

    unbang :: String -> String
    unbang x = subRegex (mkRegex $ bangRegex x) query ""

    result :: String -> String -> Maybe String
    result bang url = Just $ url ++ (unbang bang)

    bangRegex :: String -> String
    bangRegex bang = "( *!" ++ bang ++ "( +|$))"
