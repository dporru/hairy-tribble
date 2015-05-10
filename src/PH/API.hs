module PH.API
  (
    api
  ) where

import           Common
import qualified PH.DB              as DB
import qualified PH.API.Question    as Question
import qualified PH.API.Test        as Test
import qualified PH.API.Test.Export as Export

import           Rest.Api (Api,mkVersion,Some1(Some1),Router,root,route,(-/),(--/))
import           Rest.Dictionary.Combinators
import qualified Rest.Handler       as R
import qualified Rest.Resource      as R
import qualified Rest.Schema        as R

api :: Api IO
api = [(mkVersion 0 0 0,Some1 ph)]

ph :: Router IO IO
ph = root -/ route Question.resource
          -/ route Test.resource
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
