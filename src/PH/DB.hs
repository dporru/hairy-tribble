module PH.DB where

import           Common
import           PH.Types
-- import           PH.Types.JSON ()
import           PH.Types.Storage

import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.IndexQuery  as T


initialiseIndices :: IO ()
initialiseIndices = do
  T.index (ID.selector :: ID.WithID Question -> ID.ID Question)
  T.index (ID.selector :: ID.WithID Test     -> ID.ID Test    )

run :: (MonadIO m) => STM a -> m a
run = liftIO . T.atomicallySync

flush :: STM ()
flush = T.flushAll
