{-# LANGUAGE TemplateHaskell #-}
module PH.Types
  (
    Question(..),question,answer
  , Answer(..),correct,incorrect,order
  , AnswerOrder
  , Test(..),name,questions
  , module PH.Types.Dated
  , module PH.Types.Labelled
  , Decorated
  , undecorated
  ) where

import           Common
import qualified Data.TCache.ID as ID
import           PH.Types.Dated
import           PH.Types.Labelled


data Question
  = Question
    {
      _question      :: Text
    , _answer        :: Answer
    }
  deriving (Generic,Typeable,Show)

data Answer
  = Open Text
  | MultipleChoice
    {
      _correct   :: Text
    , _incorrect :: [Text]
    , _order     :: AnswerOrder
    }
  deriving (Generic,Typeable,Show)

type AnswerOrder
  = [Int]

data Test
  = Test
    {
      _name      :: Text
    , _questions :: [ID.Ref (Decorated Question)]
    }
  deriving (Generic,Typeable,Show)

type Decorated x
  = Labelled (Dated x)

makeLenses ''Question
makeLenses ''Answer
makeLenses ''Test

undecorated :: Lens' (Decorated x) x
undecorated = withoutLabels . withoutDates
