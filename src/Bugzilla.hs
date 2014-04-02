{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bugzilla
( bzRequests
, BzRequest (..)
, reqBug
, reqComments
) where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Web.Bugzilla
import Web.Bugzilla.Search

data BzRequest = NeedinfoRequest Bug [Comment] Flag
               | ReviewRequest Bug [Comment] Attachment
               | FeedbackRequest Bug [Comment] Attachment

reqBug :: BzRequest -> Bug
reqBug (NeedinfoRequest bug _ _) = bug
reqBug (ReviewRequest bug _ _) = bug
reqBug (FeedbackRequest bug _ _) = bug

reqComments :: BzRequest -> [Comment]
reqComments (NeedinfoRequest _ cs _) = cs
reqComments (ReviewRequest _ cs _) = cs
reqComments (FeedbackRequest _ cs _) = cs

bzRequests :: UserEmail -> Maybe T.Text -> IO [BzRequest]
bzRequests user pass = withBz user pass $ doRequests user

takeLastReversed :: Int -> [a] -> [a]
takeLastReversed n = take n . reverse

recentComments :: [Comment] -> [Comment]
recentComments = takeLastReversed 10 . filter (\c -> commentCreator c /= "tbplbot@gmail.com")

doRequests :: UserEmail -> BugzillaSession -> IO [BzRequest]
doRequests user session = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    needinfoReqs <- forM needinfoBugs $ \bug -> do
      let flags = filter hasNeedinfoFlag (bugFlags bug)
      case flags of
        [flag] -> do comments <- recentComments <$> getComments session (bugId bug)
                     return [NeedinfoRequest bug comments flag]
        _      -> return []

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- searchBugs session reviewSearch
    rAndFReqs <- forM reviewBugs $ \bug -> do
      attachments <- getAttachments session (bugId bug)
      comments <- recentComments <$> getComments session (bugId bug)
      let reviewAttachments = filter (any hasReviewFlag . attachmentFlags) attachments
          reviewReqs :: [BzRequest]
          reviewReqs = map (ReviewRequest bug comments ) reviewAttachments
          feedbackAttachments = filter (any hasFeedbackFlag . attachmentFlags) attachments
          feedbackReqs :: [BzRequest]
          feedbackReqs = map (FeedbackRequest bug comments) feedbackAttachments
      return $ reviewReqs ++ feedbackReqs

    return $ (concat needinfoReqs) ++ (concat rAndFReqs)

  where

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"

withBz :: UserEmail -> Maybe T.Text -> (BugzillaSession -> IO a) -> IO a
withBz user mPassword f = do
  withBugzillaContext "bugzilla.mozilla.org" $ \ctx ->
    case mPassword of
      Just password ->
        do mSession <- loginSession ctx user password
           case mSession of
             Just session -> f session
             Nothing      -> f $ anonymousSession ctx
      Nothing -> f $ anonymousSession ctx
