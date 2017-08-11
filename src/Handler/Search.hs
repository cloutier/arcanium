{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Search where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Regex.Posix

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
	let searchEngine = "https://duckduckgo.com/?q=" :: String
	searchTermsMaybe <- lookupGetParams "q"
	let searchTerms = intercalate " " $ map unpack searchTermsMaybe
        let searchEngine2 = findBangs searchTerms
	let url = searchEngine2 ++ searchTerms
        -- bangs <- findBangs searchTerms

	$logDebug "test"
        -- $logDebug test 
	-- $logDebug $ searchTermsMaybe
	-- defaultLayout $ do
	    -- let searchTerm = "Welcome To Yesod!" :: String
	  --  let searchTerm = test2
	  --  $(widgetFile "search")
	redirect $ url 

findBangs :: String -> String
findBangs query
  | matches "( *!g +)" = "https://google.com/search?q="
  | matches "( *!b +)" = "https://www.bing.com/search?q="
  | otherwise = "https://google.com/search?q="
  where
    matches :: String -> Bool
    matches a = query =~ a :: Bool
