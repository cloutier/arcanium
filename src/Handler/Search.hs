{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Search where

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



let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
  in share [mkPersist mongoSettings] [persistLowerCase| 
Queries
    terms String
    ip Text Maybe
    time UTCTime default=CURRENT_TIME
    deriving (Show)
|]

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getSearchR :: Handler ()
getSearchR = do
	time <- liftIO getCurrentTime
	maybeIp <- lookupHeader "X-Real-IP"
	let ip = fmap (decodeASCII) maybeIp
	let searchEngine = "https://google.com/search?q=" :: String
	searchTermsMaybe <- lookupGetParams "q"
	let searchTerms = intercalate " " $ map unpack searchTermsMaybe
	withMongoDBConn
	    "test"
	    "localhost"
	    (PortNumber 27017)
	    Nothing
	    2000
	    (runMongoDBPool
		master
		(do user <- insert $ Queries searchTerms ip time
		    liftIO $ print user
		    return ()))
        let searchEngine2 = findBangs searchTerms
        -- let searchEngine3 = fmap (++ searchTerms) searchEngine2
	case searchEngine2 of Just x -> redirect (x :: String)
	     		      Nothing -> $logDebug "no redirect"

	$logDebug "test"
	-- defaultLayout $ do
	    -- let searchTerm = "Welcome To Yesod!" :: String
	  --  $(widgetFile "search")
	redirect (searchEngine ++ searchTerms)

findBangs :: String -> Maybe String
findBangs query
  | matches "g" = result "g" "https://google.com/search?q="
  | matches "b" = result "b" "https://www.bing.com/search?q="
  | matches "ddg" = result "ddg" "https://www.duckduckgo.com/?q="
  | matches "wen" = result "wen" "https://en.wikipedia.org/w/index.php?search="
  | matches "wfr" = result "wfr" "https://fr.wikipedia.org/w/index.php?search="
  | matches "aca" = result "aca" "https://www.amazon.ca/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
  | matches "wikidata" = result "wikidata" "https://www.wikidata.org/w/index.php?search="
  | matches "gh" = result "gh" "https://github.com/search?utf8=%E2%9C%93&q="
  | otherwise = Nothing
  where
    matches :: String -> Bool
    matches a = query =~ (bangRegex a) :: Bool

    unbang :: String -> String
    unbang match = subRegex (mkRegex $ bangRegex match) query ""

    result :: String -> String -> Maybe String
    result bang url = Just $ url ++ (unbang bang)

    bangRegex :: String -> String
    bangRegex bang = "( *!" ++ bang ++ "( +|$))"
