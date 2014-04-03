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
import Control.Arrow ((&&&), second)
import Control.Monad
import Data.Function
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import Data.List
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
               | AssignedRequest Bug [Comment] [Attachment]
               | PendingRequest Bug [Comment] [Attachment] -- Assigned bugs waiting on r? or f?
               | ReviewedRequest Bug [Comment] [Attachment] -- Assigned bugs with r+/r-/f+/f-
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
reqBug (ReviewRequest bug _ _)   = bug
reqBug (FeedbackRequest bug _ _) = bug
reqBug (AssignedRequest bug _ _) = bug
reqBug (PendingRequest bug _ _)  = bug
reqBug (ReviewedRequest bug _ _) = bug

reqComments :: BzRequest -> [Comment]
reqComments (NeedinfoRequest _ cs _) = cs
reqComments (ReviewRequest _ cs _)   = cs
reqComments (FeedbackRequest _ cs _) = cs
reqComments (AssignedRequest _ cs _) = cs
reqComments (PendingRequest _ cs _)  = cs
reqComments (ReviewedRequest _ cs _) = cs

takeLastReversed :: Int -> [a] -> [a]
takeLastReversed n = take n . reverse

recentComments :: [Comment] -> [Comment]
recentComments = takeLastReversed 10
               . filter ((/= "tbplbot@gmail.com") . commentCreator)

categorize :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
categorize f = map (fst . head &&& map snd)
                   . groupBy ((==) `on` fst)
                   . sortBy (compare `on` fst)
                   . map (f &&& id)

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
      attachments <- filter attachmentIsPatch <$> getAttachments session (bugId bug)
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
      putStrLn $ "Getting metadata for assigned bug " ++ show (bugId bug)
      comments <- recentComments <$> getComments session (bugId bug)
      attachments <- filter attachmentIsPatch <$> getAttachments session (bugId bug)
      let attsByFile = attachmentsByFile attachments
          newestAtts = map snd . newestNonobsoleteByFile $ attsByFile
          hasPendingAtts = any hasPendingFlags attsByFile
          hasReviewedAtts = (not . null $ attsByFile) && all hasReviewedFlags attsByFile
      return $ case (hasPendingAtts, hasReviewedAtts) of
        (True, _) -> PendingRequest bug comments newestAtts
        (_, True) -> ReviewedRequest bug comments newestAtts
        _         -> AssignedRequest bug comments newestAtts

    return $ (concat needinfoReqs) ++ (concat rAndFReqs) ++ assignedReqs

  where

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"


    attachmentsByFile = categorize attachmentFileName
                      . filter (not . attachmentIsObsolete)
    
    newestAttachment = head . sortBy (flip (compare `on` attachmentCreationTime))

    newestNonobsoleteByFile = filter (not . attachmentIsObsolete . snd)
                            . map (second newestAttachment)

    hasPendingFlags :: (T.Text, [Attachment]) -> Bool
    hasPendingFlags (_, atts) =
        (`any` atts) $ \att ->
          (`any` attachmentFlags att) $ \f ->
            isRequestedFlag f && (isReviewFlag f || isFeedbackFlag f)

    -- TODO: This doesn't handle the 'unset review' idiom. Need to
    -- look at history for that. We want cases where the requestee
    -- unset the flag.
    hasReviewedFlags :: (T.Text, [Attachment]) -> Bool
    hasReviewedFlags (_, atts) =
        (`any` atts) $ \att ->
          (`any` attachmentFlags att) $ \f ->
            (isPositiveFlag f || isNegativeFlag f) &&
            (isReviewFlag f || isFeedbackFlag f)

    isRequestedFlag = (== "?") . flagStatus
    isNegativeFlag = (== "-") . flagStatus
    isPositiveFlag = (== "+") . flagStatus
    isReviewFlag    = (== "review") . flagName
    isFeedbackFlag  = (== "feedback") . flagName
