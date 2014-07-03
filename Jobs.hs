module Jobs
( Job (..)
, JobQueue
, spawnWorkers
, queueJob
) where

import Prelude
import Model

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Monoid ((<>))
import qualified Data.Sequence as S
import Database.Persist

import Settings (PersistConf)

import Data.Time (getCurrentTime)
import System.Random (randomRIO)

-- The intention is for Job to be a sum type, with different `perform` implementations
--   for each constructor. For now, we just have the one:
data Job = RunFeedJob FeedId

-- A job queue is simply a sequence of jobs that multiple threads can access safely (using STM)
type JobQueue = TVar (S.Seq Job)

-- Basic helpers for using a Sequence as a queue
enqueue :: S.Seq a -> a -> S.Seq a
enqueue xs x = xs S.|> x

dequeue :: S.Seq a -> Maybe (a, S.Seq a)
dequeue s = case S.viewl s of
  x S.:< xs -> Just (x, xs)
  _         -> Nothing


-- The public API for queueing a job
queueJob :: JobQueue -> Job -> IO ()
queueJob qvar j = atomically . modifyTVar qvar $ \v -> enqueue v j

dequeueJob :: JobQueue -> IO (Maybe Job)
dequeueJob qvar = atomically $ do
  q <- readTVar qvar
  case dequeue q of
    Just (x,xs) -> do
      writeTVar qvar xs
      return $ Just x
    Nothing -> return Nothing

-- Starts an empty job queue and some number of workers to consume from that queue
spawnWorkers :: PersistConfigPool PersistConf -> PersistConf -> Int -> IO JobQueue
spawnWorkers pool dbconf n = do
  q <- atomically $ newTVar S.empty
  replicateM_ n . forkIO . forever $ do
    qi <- dequeueJob q
    case qi of
      Just i -> perform pool dbconf i
      Nothing -> threadDelay 1000000
  return q

-- This allows us to run db queries inside a worker, similar to runDB inside a Handler
runDBIO pool dbconf f = runStdoutLoggingT . runResourceT $ runPool dbconf f pool

-- `perform` defines the actual work to be done for each type of job
-- TODO: figure out monadic sugar so that we can use e.g. runW and not need to pass in
--       pool and dbconf
--   also, figure out a getBy404 equivalent
--   also also, hook in logging (w/ numbered workers?)
perform pool dbconf (RunFeedJob _id) = do
  now <- liftIO getCurrentTime
  liftIO . putStrLn $ (show now) <> "  -- Trying " <> (show _id)
  mfeed <- runDBIO pool dbconf . get $ _id
  liftIO $ case mfeed of
    Just feed -> do
      putStrLn $ (show now) <> "  -- Running feed '" <> (show $ feedUrl feed) <> "'"
      -- Pretend these are variably complicated units of work
      sleep <- randomRIO (1,10)
      threadDelay $ sleep * 1000000
    Nothing -> return ()
