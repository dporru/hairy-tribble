module PH.Types.Indices where


import           Common
import           PH.Types
import           PH.Types.Storage ()

import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.Defs        as T
import qualified Data.TCache.Index       as T
import qualified Data.TCache.Index.Map   as IndexMap


initialiseIndices :: T.Persist -> IO ()
initialiseIndices store = do
  T.index store questionID
  T.index store testID
  T.index store questionLabels
  T.index store testLabels

questionID :: IndexMap.Field (ID.WithID (Decorated Question)) Identity (ID.ID (Decorated Question))
questionID = idIndex
testID :: IndexMap.Field (ID.WithID (Decorated Test)) Identity (ID.ID (Decorated Test))
testID = idIndex
questionLabels :: IndexMap.Field (ID.WithID (Decorated Question)) Set.Set Text
questionLabels = labelsIndex
testLabels :: IndexMap.Field (ID.WithID (Decorated Test)) Set.Set Text
testLabels = labelsIndex

labelsIndex :: IndexMap.Field (ID.WithID (Decorated o)) Set.Set Text
labelsIndex = IndexMap.namedFields (view labels . ID._object) "labels"

idIndex :: IndexMap.Field (ID.WithID (Decorated o)) Identity (ID.ID (Decorated o))
idIndex = IndexMap.field ID.__ID

instance T.Indexable (Map.Map k v) where
  key = const ""
