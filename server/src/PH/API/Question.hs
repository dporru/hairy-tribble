module PH.API.Question
  (
    resource
  ) where

import           Common
import           PH.Types

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.TCache   as T
import           Rest (Void,Range,jsonO,singleBy,named,withListing,Handler,ListHandler,mkIdHandler,mkListing)
import qualified Rest.Resource as R


resource :: R.Resource IO (ReaderT (RefID Question) IO) (RefID Question) () Void
resource = R.mkResourceReader
  { R.name   = "question"
  , R.schema = withListing () $ named [("id",singleBy T.getDBRef)]
  , R.list   = const list
  , R.get    = Just get
  }

get :: Handler (ReaderT (RefID Question) IO)
get = mkIdHandler jsonO $ \_ qid -> liftIO $ readQuestionFromDb qid

readQuestionFromDb :: RefID Question -> IO Question
readQuestionFromDb _ = return testVraag

testVraag :: Question
testVraag = Question { question = "Is dit een testvraag?", answer = Open "Ja!" }

list :: ListHandler IO
list = mkListing jsonO $ \range -> lift $ readQuestions range

readQuestions :: Range -> IO [Question]
readQuestions _ = return [testVraag]
