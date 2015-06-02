module PH.API
  (
    api
  ) where

import           Common
import qualified PH.API.Test.Export as Export
import qualified PH.DB              as DB
import           PH.Types

import           Rest.Api (Api,mkVersion,Some1(Some1),Router,root,route,(-/),(--/))
import           Rest.Dictionary.Combinators
import qualified Rest.Handler       as R
import qualified Rest.Resource      as R
import qualified Rest.Schema        as R
import qualified Rest.TCache.ID     as ID

api :: Api IO
api = [(mkVersion 0 0 0,Some1 ph)]

ph :: Router IO IO
ph = root -/ route (ID.resource "question" :: ID.Resource Question)
          -/ route (ID.resource "test"     :: ID.Resource Test)
            --/ route Export.resource
          -/ route admin

admin :: R.Resource IO IO () R.Void R.Void
admin = R.mkResourceId
  {
    R.name = "admin"
  , R.schema = R.noListing $ R.named [("flush",R.single ())]
  , R.get = Just $ R.mkInputHandler id $ \ _ -> liftIO $ do
      putStrLn "Flushing database..."
      DB.run DB.flush
  }
