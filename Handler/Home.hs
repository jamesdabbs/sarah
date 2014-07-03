{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Jobs

-- Simple helper for throwing jobs on to the application's queue
queue :: Job -> Handler ()
queue job = do
  app <- getYesod
  liftIO $ enqueueJob (jobQueue app) job

getHomeR :: Handler Html
getHomeR = do
  -- Test out the workers on several dummy jobs, some of which
  --   are deleted by the time the workers run
  feeds <- mapM createFeed [1..10]
  mapM_ (runDB . delete) $ take 5 feeds
  mapM_ (queue . RunFeedJob) feeds

  defaultLayout $ do
    setTitle "Worker test"
    $(widgetFile "homepage")

  where
    createFeed :: Int -> Handler FeedId
    createFeed n = do
      now <- liftIO getCurrentTime
      runDB . insert $ Feed url now now now
      where url = T.pack $ "this is feed url #" ++ show n
