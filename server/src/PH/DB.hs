module PH.DB where

import           Common
import           PH.Types
import           PH.Types.JSON ()

import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.IndexQuery  as T


initialiseIndices :: IO ()
initialiseIndices = do
  T.index (ID.selector :: ID.WithID Question -> ID.ID)
  T.index (ID.selector :: ID.WithID Test     -> ID.ID)

run :: STM a -> IO a
run = T.atomicallySync
