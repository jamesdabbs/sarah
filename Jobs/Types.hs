module Jobs.Types
( Job (..)
) where

import Model

-- FIXME: I'd prefer that this was a type associated with the
--   YesodWorker instance, but that seems to be causing some
--   circular declarations
data Job = RunFeedJob FeedId
