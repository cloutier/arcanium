{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Search where

import Import
import System.Environment
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist 
import Database.Persist.TH
import Database.Persist.MongoDB 
import Language.Haskell.TH.Syntax
import Network (PortID (PortNumber))
import Network.Wai
import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Time
import Import.Bangs (findBangs)
import Network.Wreq
import Data.Aeson 
import Control.Lens
import Data.Aeson.Lens 


let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
  in share [mkPersist mongoSettings] [persistLowerCase| 
Queries
    terms String
    ip String
    lang [Text] 
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
getSearchR :: Handler Html
getSearchR = do
	time <- liftIO getCurrentTime
	maybeDNT <- lookupHeader "DNT"
	maybeLang <- languages
        let bingToken = ""
        ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
        --tmp <- lookupHeaders
        -- let lookupHeader = fmap listToMaybe . lookupHeaders
        liftIO $ print ip
	let searchEngine = "https://google.com/search?q=" :: String
	searchTermsMaybe <- lookupGetParams "q"
	arcaniumFlag <- lookupGetParams "arcanium"
	let searchTerms = intercalate " " $ map unpack searchTermsMaybe
	let arca = intercalate " " $ map unpack arcaniumFlag
        liftIO $ print arca
	withMongoDBConn
	    "arcanium"
	    "localhost"
	    (PortNumber 27017)
	    Nothing
	    2000
	    (runMongoDBPool
		master
		(do user <- insert $ Queries searchTerms ip maybeLang time
		    liftIO $ print user
		    return ()))
        let searchEngine2 = findBangs searchTerms
	case searchEngine2 of Just x -> redirect (x :: String)
	     		      Nothing -> $logDebug "no redirect"

	$logDebug "test"
	case arca of
          ""  -> redirect (searchEngine ++ searchTerms) 
          _ -> $logDebug "Using built in results"
	
        let opts = defaults & header "Ocp-Apim-Subscription-Key" .~ [bingToken]
        bingResReq <- Import.lift $ Network.Wreq.getWith opts "https://api.cognitive.microsoft.com/bing/v5.0/search?q=sailing+lessons+seattle&mkt=en-us"
        let bingRes = bingResReq ^. Network.Wreq.responseBody 
	defaultLayout $ do
	    let searchTerm = "Welcome To Yesod!" :: String
	    let searchRes = show bingRes
	    $(widgetFile "search")


