{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module PH.Types.InOut where

import           Common
import qualified Data.TCache.ID   as ID
import           PH.Types

import qualified Data.Set         as Set

-- Convert to and from a flatter datatype.

class RestOut x where
  type Out x
  toOut :: x -> Out x

data DecoratedOut a
  = DecoratedOut
  {
    __id               :: ID.ID (Decorated a)
  , __labels           :: Set.Set Label
  , __object           :: a
  , __creationDate     :: UTCTime
  , __modificationDate :: UTCTime
  }
  deriving (Generic,Typeable,Show)

instance (RestOut a) => RestOut (ID.WithID (Decorated a)) where
  type Out (ID.WithID (Decorated a)) = DecoratedOut (Out a)
  toOut x = DecoratedOut
    {
      __id = castID $ ID.__ID x
    , __labels = _labels object
    , __creationDate = _creationDate . _dates $ wl
    , __modificationDate = _modificationDate . _dates $ wl
    , __object = toOut wd
    }
    where
      object = ID._object x
      wl = _withoutLabels object
      wd = _withoutDates wl
      castID :: ID.ID (Decorated a) -> ID.ID (Decorated (Out a))
      castID (ID.ID t) = ID.ID t

instance RestOut Test where
  type Out Test = Test
  toOut = id

instance RestOut Question where
  type Out Question = Question
  toOut = id

data LabelledIn a
  = LabelledIn
  {
    labels__           :: Set.Set Label
  , object__           :: a
  }
  deriving (Generic,Typeable,Show)

class RestIn x where
  type In x
  fromIn :: In x -> x

instance (RestIn a) => RestIn (Labelled a) where
  type In (Labelled a) = LabelledIn (In a)
  fromIn i = Labelled
    {
      _labels = labels__ i
    , _withoutLabels = fromIn $ object__ i
    }

instance RestIn Question where
  type In Question = Question
  fromIn q = over title generate q where
    generate t = case view generated t of
      True  -> generateTitle $ view question q
      False -> t

instance RestIn Test where
  type In Test = Test
  fromIn = id
