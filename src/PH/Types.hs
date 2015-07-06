module PH.Types
  (
    Question(..)
  , Answer(..)
  , AnswerOrder
  , Test(..)
  , module PH.Types.Dated
  ) where

import           Common
import qualified Data.TCache.ID as ID
import           PH.Types.Dated


data Question
  = Question
    {
      question      :: Text
    , answer        :: Answer
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
    , questions :: [ID.Ref (Dated Question)]
    }
  deriving (Generic,Typeable,Show)
