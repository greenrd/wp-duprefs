{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Programmatic access to (parts of) the Mediawiki API, using wreq
module Network.Mediawiki.API ( API
                             , APIConnection(..)
                             , CatMember(..)
                             , categoryMembers
                             , defaultEndpoint
                             , RevID(..)
                             , revisionEditor
                             , revisionEditors
                             , revisionIDs
                             , revisionText
                             , revisionTexts
                             , RevText(..)
                             , User(..)) where

import           BasicPrelude hiding (empty, Vector)
import           Control.Lens
import           Data.Aeson.Lens (_JSON, key, values)
import           Data.Aeson.TH hiding (Options)
import qualified Data.ByteString as BS
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Machine.Type (MachineT, runT)
import           Data.Machine.Process ((~>), asParts, autoM)
import           Data.Text (pack, unpack)
import           Data.Time.Clock (UTCTime)
import           Data.Time.ISO8601
import           Network.Mediawiki.API.Lowlevel
import qualified Prelude as P
import           System.Log.Logger

data CatMember = CatMember { title :: Text
                           , timestamp :: UTCTime
                           } deriving (Show)

$(deriveJSON defaultOptions ''CatMember)

-- XXX: Handle lag timeout better
categoryMembers :: forall k. Text -> IntSet -> MachineT API k CatMember
categoryMembers catName namespaces =
  let m1 = paged (\optsFn ->
                    do liftIO $ infoM "Network.Mediawiki.API" "Getting next batch..."
                       query (optsFn .
                         (mwParam "list" .~ ["categorymembers"]) .
                         (mwParam "cmtitle" .~ ["Category:" ++ catName]) .
                         (mwParam "cmprop" .~ ["title", "timestamp"]) .
                         (mwParam "cmnamespace" .~ map show (IntSet.toList namespaces)) .
                         (mwParam "cmsort" .~ ["timestamp"]) .
                         (mwParam "cmdir" .~ ["newer"])))
      decode :: Monad m => ByteString -> m [CatMember]
      decode = maybe (fail "wrong JSON") return . (^? key "query" . key "categorymembers" . _JSON)
  in m1 ~> autoM decode ~> asParts

newtype RevID = RevID { revid :: Int64 }

instance P.Show RevID where
  show (RevID r) = P.show r

$(deriveJSON defaultOptions ''RevID)

revisionIDs :: forall k. Text -> UTCTime -> MachineT API k RevID
revisionIDs pageName start =
  let m1 = paged (\optsFn ->
                    do liftIO . infoM "Network.Mediawiki.API" . unpack $ "Listing revisions of " ++ pageName
                       query (optsFn .
                         (mwParam "prop" .~ ["revisions"]) .
                         (mwParam "titles" .~ [pageName]) .
                         (mwParam "rvprop" .~ ["ids"]) .
                         (mwParam "rvstart" .~ [pack $ formatISO8601 start]) .
                         (mwParam "rvlimit" .~ ["max"])))
      decode :: MonadIO m => ByteString -> m [RevID]
      decode bs = do
        liftIO $ BS.writeFile "/tmp/revisionIDs.json" bs
        maybe (fail "wrong JSON") return . (^? key "query" . key "pages" . values . key "revisions" . _JSON) $ bs
  in m1 ~> autoM decode ~> asParts

newtype RevText = RevText { content :: Text }

$(deriveJSON defaultOptions ''RevText)

revisionTexts :: forall k. [RevID] -> Bool -> MachineT API k RevText
revisionTexts revIDs expandTemplates =
  let m1 = paged (\optsFn ->
                    do liftIO . infoM "Network.Mediawiki.API" $ "Getting revision texts"
                       query (optsFn .
                         (mwParam "prop" .~ ["revisions"]) .
                         (mwParam "revids" .~ map show revIDs) .
                         (mwParam "rvprop" .~ ["content"]) .
                         (mwFlag "rvexpandtemplates" .~ expandTemplates)))
      decode :: MonadIO m => ByteString -> m [RevText]
      decode bs = do
        liftIO $ BS.writeFile "/tmp/revisionTexts.json" bs
        maybe (fail "wrong JSON") return . (^? key "query" . key "pages" . values . key "revisions" . _JSON) $ bs
  in m1 ~> autoM decode ~> asParts

revisionText :: RevID -> Bool -> API RevText
revisionText revID expandTemplates = head <$> runT (revisionTexts [revID] expandTemplates)

newtype User = User { user :: Text }
             
instance P.Show User where
  show (User u) = unpack u

$(deriveJSON defaultOptions ''User)

revisionEditors :: forall k. [RevID] -> MachineT API k User
revisionEditors revIDs =
  let m1 = paged (\optsFn ->
                    do liftIO . infoM "Network.Mediawiki.API" $ "Getting revision authors"
                       query (optsFn .
                         (mwParam "prop" .~ ["revisions"]) .
                         (mwParam "revids" .~ map show revIDs) .
                         (mwParam "rvprop" .~ ["user"])))
      decode :: Monad m => ByteString -> m [User]
      decode = maybe (fail "wrong JSON") return . (^? key "query" . key "pages" . values . key "revisions" . _JSON)
  in m1 ~> autoM decode ~> asParts

revisionEditor :: RevID -> API User
revisionEditor revID = head <$> runT (revisionEditors [revID])
