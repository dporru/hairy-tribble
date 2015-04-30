module PH.Types where

import           Common
import qualified Data.TCache.ID as ID


data Question
  = Question
    {
      question :: Text
    , answer   :: Answer
    }
  deriving (Generic,Typeable,Show)

testVraag :: Question
testVraag = Question { question = "Is dit een testvraag?", answer = Open "Ja!" }

data Answer
  = Open Text
  | MultipleChoice Text [Text]
  deriving (Generic,Typeable,Show)

data Test
  = Test
    {
      name      :: Text
    , questions :: [ID.Ref Question]
    }
  deriving (Generic,Typeable,Show)

testToets :: Test
testToets = Test { name = "Testtoets", questions = [] }
