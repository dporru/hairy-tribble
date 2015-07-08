module PH.DB where

import           Common
import           PH.Types
import           PH.Types.Storage

import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Data.TCache             as T
import qualified Data.TCache.Defs        as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.Index       as T
import qualified Data.TCache.Index.Map   as IndexMap


initialiseIndices :: IO ()
initialiseIndices = do
  T.index questionID
  T.index testID
  T.index questionLabels
  T.index testLabels

questionID :: IndexMap.Field (ID.WithID (Decorated Question)) Identity (ID.ID (Decorated Question))
questionID = IndexMap.field ID.__ID
testID :: IndexMap.Field (ID.WithID (Decorated Test)) Identity (ID.ID (Decorated Test))
testID = IndexMap.field ID.__ID
questionLabels :: IndexMap.Field (ID.WithID (Decorated Question)) Set.Set Text
questionLabels = labelsIndex
testLabels :: IndexMap.Field (ID.WithID (Decorated Test)) Set.Set Text
testLabels = labelsIndex
labelsIndex = IndexMap.namedFields (view labels . ID._object) "labels"

run :: (MonadIO m) => STM a -> m a
run = liftIO . T.atomicallySync

flush :: STM ()
flush = T.flushAll

instance T.Indexable (Map.Map k v) where
  key = const ""
