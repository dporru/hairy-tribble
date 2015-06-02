module PH.API.Test
  (
    resource
  ) where

import           Common
import qualified PH.DB as DB
import           PH.Types
import           PH.Types.JSON

import qualified Data.TCache    as T
import qualified Data.TCache.ID as ID
import qualified Rest                  as R
import           Rest (Void)
import qualified Rest.Handler          as R
import qualified Rest.Resource as R


resource :: R.Resource IO (ReaderT (ID.Ref Test) IO) (ID.Ref Test) () Void
resource = R.mkResourceReader
  {
    R.name   = "test"
  , R.schema = R.withListing () $ R.unnamedSingle T.getDBRef
  , R.list   = const list
  , R.get    = Just get
  , R.update = Just update
  , R.create = Just create
  , R.remove = Just remove
  }

get :: R.Handler (ReaderT (ID.Ref Test) IO)
get = R.mkIdHandler R.jsonO $ \ _ -> liftIO . DB.run . ID.maybeDeref

list :: R.ListHandler IO
list = R.mkListing R.jsonO $ \ range -> lift $
  takeRange range <$> DB.run (ID.listWithID :: STM [ID.WithID Test])

create :: R.Handler IO
create = R.mkInputHandler (R.jsonI . R.jsonO) $ liftIO . DB.run . (ID.newRef :: Test -> STM (ID.Ref Test))

remove :: R.Handler (ReaderT (ID.Ref Test) IO)
remove = R.mkIdHandler id $ \ _ -> liftIO . DB.run . ID.delete

update :: R.Handler (ReaderT (ID.Ref Test) IO)
update = R.mkIdHandler R.jsonI $ \ (x :: Test) i -> do
  maybe (throwError R.NotFound) return =<< liftIO (DB.run $ ID.update i x)
