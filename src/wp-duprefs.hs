{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           BasicPrelude          hiding (group, null)
import           Control.Lens          ((.~))
import           Control.Monad.Reader  (runReaderT)
import qualified Data.IntSet           as IntSet
import           Data.List.Unique      (repeated)
import           Data.Machine          hiding (repeated)
import           Data.Maybe            (fromJust)
import           Data.Set              (fromList, isSubsetOf, null)
import qualified Data.Text             as T
import           Data.Text.ICU         (findAll, group)
import           Data.Vector.Unboxed   ((!))
import qualified Data.Vector.Unboxed   as V
import           Network.Mediawiki.API
import           Network.Wreq          (Options)
import           Network.Wreq.Lens     (header)
import           Network.Wreq.Session  (withAPISession)
import           System.Log.Logger

type ErrorDetector a = RevText -> Set a

-- | A set of duplicate reference names
dupRefs :: ErrorDetector Text
dupRefs = fromList . repeated . map (normalize . fromJust . group 1) . findRefs . content
  where
    findRefs = findAll "<\\s*ref\\s+name\\s*=(\"[^\"]*\"|[a-zA-Z0-9!$%&()*,\\-.:;<@\\[\\]\\^_`{|}~]+)\\s*>"
    normalize t | "\"" `T.isPrefixOf` t = read t
                | otherwise = t

-- | Does a binary search through revisions to find the revision that introduced one of the original errors
mwBlame :: forall a. Ord a => ErrorDetector a -> ProcessT API CatMember ()
mwBlame ed = autoM blameCM
  where blameCM cm =
          do
             revIDs <- runT $ revisionIDs (title cm) (timestamp cm)
             case revIDs of
               []            -> fail "No revisions found"
               (r1ID:others) ->
                 do
                    liftIO . debugM "Main" . T.unpack $ "1st revision = " ++ show r1ID
                    r1 <- revisionText r1ID True
                    let origErrors = ed r1
                    guard . not $ null origErrors
                    earliestError <- binSearch origErrors (r1ID, V.fromList $ revid <$> others)
                    editor <- revisionEditor earliestError
                    liftIO . putStrLn $ "Dup introduced @ " ++ show earliestError ++ " by " ++ show editor
        binSearch :: Set a -> (RevID, V.Vector Int64) -> API RevID
        binSearch origErrors (earliestSoFar, others)
                  | V.null others = return earliestSoFar
                  | otherwise = do
                                   let n = V.length others
                                       m = n `div` 2
                                       pivot = RevID $ others ! m
                                   pr <- revisionText pivot True
                                   binSearch origErrors $
                                     if origErrors `isSubsetOf` ed pr
                                       then (pivot        , V.slice (succ m) (n - succ m) others)
                                       else (earliestSoFar, V.take m others)

mainMachine :: MachineT API k ()
mainMachine = categoryMembers "Pages_with_duplicate_reference_names" (IntSet.singleton 0) ~> mwBlame dupRefs

userAgent :: Options -> Options
userAgent = header "User-Agent" .~ ["wp-duprefs/0.1.0.0"]

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  withAPISession $ runReaderT (runT_ mainMachine) . APIConnection defaultEndpoint userAgent
