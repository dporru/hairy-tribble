module PH.API.Test
  (
    resource
  ) where

import           Common
import           PH.Types

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.TCache   as T
import           Rest
import qualified Rest.Resource as R


resource :: Resource IO (ReaderT (RefID Test) IO) (RefID Test) () Void
resource = mkResourceReader
  { R.name   = "test"
  , R.schema = withListing () $ named [("id",singleBy T.getDBRef)]
  , R.list   = const list
  , R.get    = Just get
  }

get :: Handler (ReaderT (RefID Test) IO)
get = mkIdHandler jsonO $ \_ qid -> liftIO $ readTestFromDb qid

readTestFromDb:: RefID Test -> IO Test
readTestFromDb _ = return testToets

testToets :: Test
testToets = Test { name = "Testtoets", questions = [] }

list :: ListHandler IO
list = mkListing jsonO $ \range -> lift $ readTests range

readTests :: Range -> IO [Test]
readTests _ = return [testToets]
