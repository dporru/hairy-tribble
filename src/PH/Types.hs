module PH.Types where

import           Common
import qualified Data.TCache.ID as ID


data Question
  = Question
    {
      question      :: Text
    , answer        :: Answer
    , questionDates :: Dates
    }
  deriving (Generic,Typeable,Show)
data Question_v0
  = Question_v0
    {
      question_v0 :: Text
    , answer_v0   :: Answer
    }
  deriving (Generic,Typeable,Show)

data Answer
  = Open Text
  | MultipleChoice
    {
      correct   :: Text
    , incorrect :: [Text]
    , order     :: AnswerOrder
    }
  deriving (Generic,Typeable,Show)

type AnswerOrder
  = [Int]

data Test
  = Test
    {
      name      :: Text
    , questions :: [ID.Ref Question]
    , testDates :: Dates
    }
  deriving (Generic,Typeable,Show)
data Test_v0
  = Test_v0
    {
      name_v0      :: Text
    , questions_v0 :: [ID.Ref Question]
    }
  deriving (Generic,Typeable,Show)

data Dates
  = Dates
    {
      creationDate     :: UTCTime
    , modificationDate :: UTCTime
    , deletionDate     :: Maybe UTCTime
    }
  deriving (Generic,Typeable,Show)

newDates :: (MonadIO m) => m Dates
newDates = do
  t <- liftIO getCurrentTime
  return $ Dates t t Nothing
