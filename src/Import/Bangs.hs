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
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix
import Database.Persist 
import Database.Persist.TH
import Database.Persist.MongoDB 
import Language.Haskell.TH.Syntax
import Network (PortID (PortNumber))
import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Text.Encoding
import Data.Time

findBangs :: String -> Maybe String
findBangs query
  | matches ["g", "google"] = result "g" "https://google.com/search?q="
  | matches ["b", "bing"] = result "b" "https://www.bing.com/search?q="
  | matches ["ddg"] = result "ddg" "https://www.duckduckgo.com/?q="
  | matches ["wen"] = result "wen" "https://en.wikipedia.org/w/index.php?search="
  | matches ["wfr"] = result "wfr" "https://fr.wikipedia.org/w/index.php?search="
  | matches ["aca"] = result "aca" "https://www.amazon.ca/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
  | matches ["wikidata"] = result "wikidata" "https://www.wikidata.org/w/index.php?search="
  | matches ["gh"] = result "gh" "https://github.com/search?utf8=%E2%9C%93&q="
  | matches ["tpb"] = result "tpb" "https://thepiratebay.org/search/"
  | otherwise = Nothing
  where
    matches :: [String] -> Bool
    matches a = or $ map (\x -> query =~ (bangRegex x) :: Bool) a 

    unbang :: String -> String
    unbang match = subRegex (mkRegex $ bangRegex match) query ""

    result :: String -> String -> Maybe String
    result bang url = Just $ url ++ (unbang bang)

    bangRegex :: String -> String
    bangRegex bang = "( *!" ++ bang ++ "( +|$))"
