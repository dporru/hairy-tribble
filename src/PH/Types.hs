{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PH.Types
  (
    RichText(..),plainText
  , Question(..),question,answer
  , Answer(..),choices,order
  , AnswerOrder
  , Title(..),titleText,generated
  , generateTitle
  , Test(..),name,elements
  , TestElement(..)
  , module PH.Types.Dated
  , module PH.Types.Labelled
  , Decorated
  , undecorated
  ) where

import           Common
import qualified Data.TCache.ID as ID
import           PH.Types.Dated
import           PH.Types.Labelled

import qualified Data.Text      as Text
import qualified Text.Pandoc    as Pandoc


newtype RichText
  = Pandoc [Pandoc.Block]
  deriving (Monoid,Generic,Typeable,Show)

plainText :: Text -> RichText
plainText = Pandoc . (: []) . Pandoc.Plain . (: []) . Pandoc.Str . Text.unpack

data Question
  = Question
    {
      _question      :: RichText
    , _answer        :: Answer
    , _title         :: Title
    }
  deriving (Generic,Typeable,Show)

data Answer
  = Open RichText
  | MultipleChoice
    {
      _choices :: [(Bool,RichText)]
    , _order   :: AnswerOrder
    }
  deriving (Generic,Typeable,Show)

type AnswerOrder
  = [Int]

data Title
  = Title
    {
      _titleText :: Text
    , _generated :: Bool
    }
    deriving (Generic,Typeable,Show)

generateTitle :: RichText -> Title
generateTitle (Pandoc ps) = flip Title True . Text.pack $
  Pandoc.writePlain Pandoc.def $ Pandoc.Pandoc Pandoc.nullMeta ps

data Test
  = Test
    {
      _name     :: Text
    , _elements :: [TestElement]
    }
  deriving (Generic,Typeable,Show)

data TestElement
  = TestQuestion (ID.ID (Decorated Question))
  | TestText RichText
  deriving (Generic,Typeable,Show)

type Decorated x
  = Labelled (Dated x)

makeLenses ''Question
makeLenses ''Answer
makeLenses ''Title
makeLenses ''Test
makeLenses ''TestElement

undecorated :: Lens' (Decorated x) x
undecorated = withoutLabels . withoutDates
