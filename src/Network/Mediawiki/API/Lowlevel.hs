{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Mediawiki.API.Lowlevel where

import           BasicPrelude hiding (empty)
import           Control.Lens
import           Control.Lens.Text.Extras (delimited)
import           Control.Monad.Reader
import           Data.Aeson.Lens (_Object, _String, key)
import           Data.ByteString.Lazy (toStrict)
import           Data.HashMap.Strict (empty, foldrWithKey)
import           Data.Machine (construct, MachineT, yield)
import           Data.Time.Clock (DiffTime, secondsToDiffTime)
import           Network.Wreq (defaults, Options, Response)
import           Network.Wreq.Lens (param, responseBody)
import           Network.Wreq.Session

-- | Setter which sets (and creates if necessary) the last element of a list
replacingLast :: a -> Setter' [a] a
replacingLast deflt = sets $ \f -> maybe [f deflt] (\(xs, x) -> xs ++ [f x]) . unsnoc

-- | Parameter that follows Mediawiki multi-parameter encoding rules
mwParam :: Text -> Setter' Options [Text]
mwParam name = param name . replacingLast "" . delimited "|"

-- | Boolean parameter that follows Mediawiki parameter rules
mwFlag :: Text -> Setter' Options Bool
mwFlag name = param name . sets (\f xs -> if f (not $ null xs)
                                             then [""]
                                             else [])

data APIConnection = APIConnection { endpoint :: String
                                   , optsCustomiser :: Options -> Options
                                   , session :: Session }

defaultEndpoint :: String
defaultEndpoint = "https://en.wikipedia.org/w/api.php"

defaultMaxLag :: DiffTime
defaultMaxLag = secondsToDiffTime 1

maxLag :: DiffTime -> Options -> Options
maxLag ml = mwParam "maxlag" .~ [show . floor $ toRational ml]

paged :: Monad m => ((Options -> Options) -> m (Response ByteString)) -> MachineT m k ByteString
paged getter = construct $ process empty
  where
    process fs =
      do 
         r <- lift . getter $ foldrWithKey (\k v -> ((mwParam k .~ [v ^?! _String]) .)) id fs
         let bs = r ^. responseBody
         yield bs
         maybe (return ()) process $ bs ^? key "continue" . _Object

type API = ReaderT APIConnection IO

apiGet :: (Options -> Options) -> API (Response ByteString)
apiGet optsFn = do
   apiConnection <- ask
   r <- liftIO $ getWith (defaults & mwParam "format" .~ ["json"] & mwParam "formatversion" .~ ["2"] &
                          maxLag defaultMaxLag & 
                          optsCustomiser apiConnection & optsFn)
                         (session apiConnection)
                         (endpoint apiConnection)
   return $ over responseBody toStrict r

query :: (Options -> Options) -> API (Response ByteString)
query = apiGet . (. (mwParam "action" .~ ["query"]))
