module PH.API
  (
    M
  , api
  ) where

import           Accounts      (Account)
import           Common
import qualified PH.API.ID          as ID
import qualified PH.API.Test.Export as Export
import qualified PH.DB              as DB
import           PH.Types
import           PH.Types.JSON   ()
import           PH.Types.Server (M, Config, stores)
import qualified Session

import qualified Data.Map           as Map
import           Rest.Api
  ( Api
  , mkVersion
  , Some1(Some1)
  , Router
  , root
  , route
  , (-/)
  , (--/)
  )
import           Rest.Dictionary.Combinators
import qualified Rest.Handler       as R
import qualified Rest.Resource      as R
import qualified Rest.Schema        as R

api :: Config -> Api M
api c = [(mkVersion 0 0 0,Some1 $ ph c)]

ph :: Config -> Router M M
ph c = root -/ route (ID.resource c "question" :: ID.Resource M Question)
            -/ route (ID.resource c "test"     :: ID.Resource M Test)
              --/ route (Export.resource c)
            -/ route (admin c)

admin :: Config -> R.Resource M M () R.Void R.Void
admin c = R.mkResourceId
  {
    R.name = "admin"
  , R.schema = R.noListing $ R.named [("flush",R.single ())]
  , R.get = Just $ R.mkInputHandler id $ \ _ -> do
      liftIO $ putStrLn "Flushing database caches..."
      s <- Map.elems <$> liftIO (readMVar $ view stores c)
      for_ s $ flip DB.run DB.flush
  }
