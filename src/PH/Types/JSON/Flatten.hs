{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module PH.Types.JSON.Flatten where

import           Common
import qualified Data.TCache.ID   as ID
import           PH.Types

import qualified Data.Set         as Set

-- Convert to and from a flatter datatype.
data Out a
  = Out
  {
    __id               :: ID.ID (Decorated a)
  , __labels           :: Set.Set Label
  , __object           :: a
  , __creationDate     :: UTCTime
  , __modificationDate :: UTCTime
  }
  deriving (Generic,Typeable,Show)

instance (Typeable a,ToJSON a,ToJSON (ID.ID (Decorated a))) => ToJSON (Out a) where
  toJSON = gtoJson
instance (JSONSchema a,JSONSchema (ID.ID (Decorated a))) => JSONSchema (Out a) where
  schema = gSchema

toOut :: ID.WithID (Decorated a) -> Out a
toOut x = Out
  {
    __id = ID.__ID x
  , __labels = _labels object
  , __creationDate = _creationDate . _dates $ wl
  , __modificationDate = _modificationDate . _dates $ wl
  , __object = wd
  }
  where
    object = ID._object x
    wl = _withoutLabels object
    wd = _withoutDates wl

data In a
  = In
  {
    labels__           :: Set.Set Label
  , object__           :: a
  }
  deriving (Generic,Typeable,Show)

instance (Typeable a,FromJSON a) => FromJSON (In a) where
  parseJSON = gparseJson
instance (JSONSchema a) => JSONSchema (In a) where
  schema = gSchema

fromIn :: In a -> Labelled a
fromIn i = Labelled
  {
    _labels = labels__ i
  , _withoutLabels = object__ i
  }
