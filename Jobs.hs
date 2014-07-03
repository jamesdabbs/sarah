{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Jobs
( Job (..)
, JobQueue
-- , spawnWorkers
, emptyQueue
, enqueueJob
, dequeueJob
, WorkerT (..)
, WorkerData (..)
, RunWorkerEnv (..)
) where

import Prelude
import Model

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Sequence as S

import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogLevel, LogSource, MonadLogger (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource (MonadResource (..), InternalState, runInternalState, MonadThrow (..), monadThrow)
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, toLogStr)


-- The intention is for Job to be a sum type, with different `perform` implementations
--   for each constructor. For now, we just have the one:
data Job = RunFeedJob FeedId

-- A job queue is simply a sequence of jobs that multiple threads can access safely (using STM)
type JobQueue = TVar (S.Seq Job)

data RunWorkerEnv site = RunWorkerEnv
  { rweSite     :: !site
  , rweLog      :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -- , rheOnError  :: !(ErrorResponse -> YesodApp)
  }
data WorkerData site = WorkerData
  { workerResource :: !InternalState
  , workerEnv      :: !(RunWorkerEnv site)
  }

newtype WorkerT site m a = WorkerT
  { unWorkerT :: WorkerData site -> m a
  }

instance MonadTrans (WorkerT site) where
  lift = WorkerT . const
instance Monad m => Functor (WorkerT site m) where
  fmap = liftM
instance Monad m => Applicative (WorkerT site m) where
  pure = return
  (<*>) = ap
instance MonadIO m => MonadIO (WorkerT site m) where
  liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (WorkerT site m) where
  liftBase = lift . liftBase
-- TODO: absorb the instance declarations below
instance Monad m => Monad (WorkerT site m) where
  return = WorkerT . const . return
  WorkerT x >>= f = WorkerT $ \r -> x r >>= \x' -> unWorkerT (f x') r
instance MonadBaseControl b m => MonadBaseControl b (WorkerT site m) where
  data StM (WorkerT site m) a = StH (StM m a)
  liftBaseWith f = WorkerT $ \reader ->
    liftBaseWith $ \runInBase ->
      f $ liftM StH . runInBase . (\(WorkerT r) -> r reader)
  restoreM (StH base) = WorkerT $ const $ restoreM base
instance MonadThrow m => MonadThrow (WorkerT site m) where
  throwM = lift . monadThrow
instance (MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (WorkerT site m) where
  liftResourceT f = WorkerT $ \hd -> liftIO $ runInternalState f (workerResource hd)
instance MonadIO m => MonadLogger (WorkerT site m) where
  monadLoggerLog a b c d = WorkerT $ \hd ->
    liftIO $ rweLog (workerEnv hd) a b c (toLogStr d)


-- Basic helpers for using a Sequence as a queue
enqueue :: S.Seq a -> a -> S.Seq a
enqueue = (S.|>)

dequeue :: S.Seq a -> Maybe (a, S.Seq a)
dequeue s = case S.viewl s of
  x S.:< xs -> Just (x, xs)
  _         -> Nothing


-- The public API for queueing a job
enqueueJob :: JobQueue -> Job -> IO ()
enqueueJob qvar j = atomically . modifyTVar qvar $ \v -> enqueue v j

dequeueJob :: JobQueue -> IO (Maybe Job)
dequeueJob qvar = atomically $ do
  q <- readTVar qvar
  case dequeue q of
    Just (x,xs) -> do
      writeTVar qvar xs
      return $ Just x
    Nothing -> return Nothing

emptyQueue :: IO JobQueue
emptyQueue = atomically $ newTVar S.empty
