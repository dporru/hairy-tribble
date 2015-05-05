module PH.API.Question
  (
    resource
  ) where

import           Common
import qualified PH.DB as DB
import           PH.Types
import           PH.Types.JSON

import qualified Data.TCache           as T
import qualified Data.TCache.ID        as ID
import qualified Rest                  as R
import           Rest (Void)
import qualified Rest.Handler          as R
import qualified Rest.Resource         as R


resource :: R.Resource IO (ReaderT (ID.Ref Question) IO) (ID.Ref Question) () Void
resource = R.mkResourceReader
  {
    R.name   = "question"
  , R.schema = R.withListing () $ R.named [("id",R.singleBy T.getDBRef)]
  , R.list   = const list
  , R.get    = Just get
  , R.update = Just update
  , R.create = Just create
  , R.remove = Just remove
  }

get :: R.Handler (ReaderT (ID.Ref Question) IO)
get = R.mkIdHandler R.jsonO $ \ () -> liftIO . DB.run . ID.maybeDeref

list :: R.ListHandler IO
list = R.mkListing R.jsonO $ \ range -> lift $
  takeRange range <$> DB.run (ID.listWithID :: STM [ID.WithID Question])

create :: R.Handler IO
create = R.mkInputHandler R.jsonI $ void . liftIO . DB.run . (ID.newRef :: Question -> STM (ID.Ref Question))

remove :: R.Handler (ReaderT (ID.Ref Question) IO)
remove = R.mkIdHandler id $ \ () -> liftIO . DB.run . ID.delete

update :: R.Handler (ReaderT (ID.Ref Question) IO)
update = R.mkIdHandler R.jsonI $ \ (x :: Question) i -> do
  maybe (throwError R.NotFound) return =<< liftIO (DB.run $ ID.update i x)
