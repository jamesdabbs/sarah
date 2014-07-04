module Yesod.Worker
( enqueue
, emptyQueue
, runW
, spawnWorkers
, Worker
) where

import Import
import Yesod.Worker.Queue
import Yesod.Worker.Types
import Jobs.Types (Job)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, liftM, replicateM_)
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import Database.Persist.Sql (SqlPersistT)
import qualified Database.Persist
import Yesod.Default.Config (AppConfig (..))


type Worker = WorkerT App IO

askWorkerEnv :: MonadWorker m => m (RunWorkerEnv (WorkerSite m))
askWorkerEnv = liftWorkerT $ WorkerT $ return . workerEnv

getYesodW :: MonadWorker m => m (WorkerSite m)
getYesodW = rweSite `liftM` askWorkerEnv

runW :: SqlPersistT Worker a -> Worker a
runW f = do
  app <- getYesodW
  Database.Persist.runPool (persistConfig app) f (connPool app)


runWorker :: App -> Worker a -> IO a
runWorker site worker = runResourceT . withInternalState $ \resState -> do
  let rwe = RunWorkerEnv
            { rweSite = site
            , rweLog = messageLoggerSource site $ appLogger site
            }
  let wd = WorkerData
           { workerResource = resState
           , workerEnv = rwe
           }
  -- FIXME: catch and handle errors (see unHandlerT)
  unWorkerT worker wd

spawnWorkers :: YesodWorker App => App -> IO ()
spawnWorkers site = do
  replicateM_ (extraWorkers . appExtra . settings $ site) . forkIO . forever $ do
    mj <- dequeueJob $ jobQueue site
    case mj of
      Just job -> runWorker site $ perform job
      Nothing -> threadDelay 1000000

enqueue :: Job -> Handler ()
enqueue job = do
  app <- getYesod
  liftIO $ enqueueJob (jobQueue app) job
