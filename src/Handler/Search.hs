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
import Database.Persist 
import Database.Persist.TH
import Database.Persist.MongoDB 
import Language.Haskell.TH.Syntax
import Network (PortID (PortNumber))
import Network.Wai
import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
-- import Data.Text.Encoding
import Data.Time
import Import.Bangs (findBangs)



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
getSearchR :: Handler ()
getSearchR = do
	time <- liftIO getCurrentTime
	maybeDNT <- lookupHeader "DNT"
	maybeLang <- languages
        ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
        --tmp <- lookupHeaders
        --liftIO $ print $ show maybeHead
        -- let lookupHeader = fmap listToMaybe . lookupHeaders
        liftIO $ print ip
	let searchEngine = "https://google.com/search?q=" :: String
	searchTermsMaybe <- lookupGetParams "q"
	let searchTerms = intercalate " " $ map unpack searchTermsMaybe
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
	-- defaultLayout $ do
	    -- let searchTerm = "Welcome To Yesod!" :: String
	  --  $(widgetFile "search")
	redirect (searchEngine ++ searchTerms)

