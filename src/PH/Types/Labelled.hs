{-# LANGUAGE TemplateHaskell #-}
module PH.Types.Labelled
  (
    Labelled(..),labels,withoutLabels
  ) where

import           Control.Lens.TH (makeLenses)
import qualified Data.Set     as Set
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)


data Labelled x
  = Labelled
    {
      _labels        :: Set.Set Label
    , _withoutLabels :: x
    }
  deriving (Generic,Typeable,Show)

type Label
  = Text

makeLenses ''Labelled
