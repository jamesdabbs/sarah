{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jobs
( Job (..)
, perform
) where

import Import
import Yesod.Worker.Types
import Yesod.Worker (runW)
import Jobs.Types (Job (..))

import qualified Data.Text as T


-- TODO: de-orphan this instance
instance YesodWorker App where
  perform (RunFeedJob _id) = do
    $(logDebug) $ "-- Trying " <> (T.pack $ show _id)
    mfeed <- runW . get $ _id
    case mfeed of
      Just feed -> $(logDebug) $ "-- Running feed " <> (T.pack . show $ feedUrl feed)
      Nothing -> return ()
