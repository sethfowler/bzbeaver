{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bugzilla
( bzRequests
, BzRequest (..)
, reqBug
, reqComments
, BugzillaSession
, newBzSession
) where

import Control.Applicative
import Control.Monad
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import Data.SafeCopy (base, contain, deriveSafeCopy, getCopy,
                      putCopy, safeGet, safePut, SafeCopy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Web.Bugzilla
import Web.Bugzilla.Search

newBzSession :: UserEmail -> Maybe T.Text -> IO BugzillaSession
newBzSession user mPassword = do
  ctx <- newBugzillaContext "bugzilla.mozilla.org"
  case mPassword of
    Just password ->
      do mSession <- loginSession ctx user password
         case mSession of
           Just session -> return session
           Nothing      -> return $ anonymousSession ctx
    Nothing -> return $ anonymousSession ctx
  
data BzRequest = NeedinfoRequest Bug [Comment] Flag
               | ReviewRequest Bug [Comment] Attachment
               | FeedbackRequest Bug [Comment] Attachment
               | AssignedRequest Bug [Comment]
                 deriving (Eq, Show, Typeable)

-- Manually implement SafeCopy for HashMap. We need this to derive it
-- for Bug below.
instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (H.HashMap a b) where
    getCopy = contain $ fmap H.fromList safeGet 
    putCopy = contain . safePut . H.toList

deriveSafeCopy 0 'base ''Attachment
deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''Bug
deriveSafeCopy 0 'base ''Comment
deriveSafeCopy 0 'base ''Flag
deriveSafeCopy 0 'base ''BzRequest

reqBug :: BzRequest -> Bug
reqBug (NeedinfoRequest bug _ _) = bug
reqBug (ReviewRequest bug _ _) = bug
reqBug (FeedbackRequest bug _ _) = bug
reqBug (AssignedRequest bug _) = bug

reqComments :: BzRequest -> [Comment]
reqComments (NeedinfoRequest _ cs _) = cs
reqComments (ReviewRequest _ cs _) = cs
reqComments (FeedbackRequest _ cs _) = cs
reqComments (AssignedRequest _ cs) = cs

takeLastReversed :: Int -> [a] -> [a]
takeLastReversed n = take n . reverse

recentComments :: [Comment] -> [Comment]
recentComments = takeLastReversed 10 . filter (\c -> commentCreator c /= "tbplbot@gmail.com")

bzRequests :: BugzillaSession -> UserEmail -> IO [BzRequest]
bzRequests session user = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    needinfoReqs <- forM needinfoBugs $ \bug -> do
      putStrLn $ "Getting comments for needinfo bug " ++ show (bugId bug)
      let flags = filter hasNeedinfoFlag (bugFlags bug)
      case flags of
        [flag] -> do comments <- recentComments <$> getComments session (bugId bug)
                     return [NeedinfoRequest bug comments flag]
        _      -> return []

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- searchBugs session reviewSearch
    rAndFReqs <- forM reviewBugs $ \bug -> do
      putStrLn $ "Getting metadata for review/feedback bug " ++ show (bugId bug)
      attachments <- getAttachments session (bugId bug)
      comments <- recentComments <$> getComments session (bugId bug)
      let reviewAttachments = filter (any hasReviewFlag . attachmentFlags) attachments
          reviewReqs :: [BzRequest]
          reviewReqs = map (ReviewRequest bug comments ) reviewAttachments
          feedbackAttachments = filter (any hasFeedbackFlag . attachmentFlags) attachments
          feedbackReqs :: [BzRequest]
          feedbackReqs = map (FeedbackRequest bug comments) feedbackAttachments
      return $ reviewReqs ++ feedbackReqs

    let assignedSearch = AssignedToField .==. user .&&.
                           (StatusField .==. "NEW" .||.
                            StatusField .==. "ASSIGNED" .||.
                            StatusField .==. "REOPENED")
    assignedBugs <- searchBugs session assignedSearch
    assignedReqs <- forM assignedBugs $ \bug -> do
      putStrLn $ "Getting comments for assigned bug " ++ show (bugId bug)
      comments <- recentComments <$> getComments session (bugId bug)
      return $ AssignedRequest bug comments

    return $ (concat needinfoReqs) ++ (concat rAndFReqs) ++ assignedReqs

  where

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"
