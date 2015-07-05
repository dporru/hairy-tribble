module PH.DB where

import           Common
import           PH.Types
import           PH.Types.Storage

import qualified Data.Map                as Map
import qualified Data.TCache             as T
import qualified Data.TCache.Defs        as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.Index       as T
import qualified Data.TCache.Index.Map   as IndexMap


initialiseIndices :: IO ()
initialiseIndices = do
  T.index (IndexMap.field (ID._id :: ID.WithID Question -> ID.ID Question))
  T.index (IndexMap.field (ID._id :: ID.WithID Test     -> ID.ID Test    ))

run :: (MonadIO m) => STM a -> m a
run = liftIO . T.atomicallySync

flush :: STM ()
flush = T.flushAll

instance T.Indexable (Map.Map k v) where
  key = const ""

-- instance T.Serializable (Map.Map k v) where
--   key = const ""
