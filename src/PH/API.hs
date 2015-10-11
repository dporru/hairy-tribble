module PH.API
  (
    M
  , api
  ) where

import           Common
import qualified PH.API.ID          as ID
import qualified PH.API.Test.Export as Export
import qualified PH.DB              as DB
import           PH.Types
import           PH.Types.JSON ()
import qualified Session

import qualified Happstack.Server   as H
import           Rest.Api      (Api,mkVersion,Some1(Some1),Router,root,route,(-/),(--/))
import           Rest.Dictionary.Combinators
import qualified Rest.Handler       as R
import qualified Rest.Resource      as R
import qualified Rest.Schema        as R

type M
  = Session.ServerSessionT Integer (H.ServerPartT IO)

api :: Api M
api = [(mkVersion 0 0 0,Some1 ph)]

ph :: Router M M
ph = root -/ route (ID.resource "question" :: ID.Resource M Question)
          -/ route (ID.resource "test"     :: ID.Resource M Test)
            --/ route Export.resource
          -/ route admin

admin :: R.Resource M M () R.Void R.Void
admin = R.mkResourceId
  {
    R.name = "admin"
  , R.schema = R.noListing $ R.named [("flush",R.single ())]
  , R.get = Just $ R.mkInputHandler id $ \ _ -> liftIO $ do
      putStrLn "Flushing database..."
      DB.run DB.flush
  }
