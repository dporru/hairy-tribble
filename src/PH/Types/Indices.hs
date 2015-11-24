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

questionID :: IDIndex Question
questionID = idIndex
testID :: IDIndex Test
testID = idIndex
questionLabels :: LabelsIndex Question
questionLabels = labelsIndex
testLabels :: LabelsIndex Test
testLabels = labelsIndex

type LabelsIndex o
  = IndexMap.Field (ID.WithID (Decorated o)) Set.Set Text

labelsIndex :: LabelsIndex o
labelsIndex = IndexMap.namedFields (view labels . ID._object) "labels"

type IDIndex o
  = IndexMap.Field (ID.WithID (Decorated o)) Identity (ID.ID (Decorated o))

idIndex :: IDIndex o
idIndex = IndexMap.field ID.__ID

instance T.Indexable (Map.Map k v) where
  key = const ""
