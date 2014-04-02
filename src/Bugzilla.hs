{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bugzilla
( bzRequests
, BzRequest (..)
) where

import Control.Monad
import qualified Data.Text as T
import Web.Bugzilla
import Web.Bugzilla.Search

data BzRequest = NeedinfoRequest Bug Flag
               | ReviewRequest Bug Attachment
               | FeedbackRequest Bug Attachment

bzRequests :: UserEmail -> Maybe T.Text -> IO [BzRequest]
bzRequests user pass = withBz user pass $ doRequests user

doRequests :: UserEmail -> BugzillaSession -> IO [BzRequest]
doRequests user session = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    let needinfoReqs :: [BzRequest]
        needinfoReqs = concatMap mkNeedinfoReq needinfoBugs

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- searchBugs session reviewSearch
    rAndFReqs <- forM reviewBugs $ \rBug -> do
      attachments <- getAttachments session (bugId rBug)
      let reviewAttachments = filter (any hasReviewFlag . attachmentFlags) attachments
          reviewReqs :: [BzRequest]
          reviewReqs = map (ReviewRequest rBug) reviewAttachments
          feedbackAttachments = filter (any hasFeedbackFlag . attachmentFlags) attachments
          feedbackReqs :: [BzRequest]
          feedbackReqs = map (FeedbackRequest rBug) feedbackAttachments
      return $ reviewReqs ++ feedbackReqs

    return $ needinfoReqs ++ (concat rAndFReqs)

  where

    mkNeedinfoReq bug@(Bug {..}) = do
      let flags = filter hasNeedinfoFlag bugFlags
      case flags of
        [flag] -> [NeedinfoRequest bug flag]
        _      -> []

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
