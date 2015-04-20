{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module PH.Types where

import           Common
import           ID (WithID(WithID))

import           Data.Aeson       (toJSON,parseJSON,encode,eitherDecode)
import           Data.JSON.Schema (Proxy(Proxy))
import           Data.Text        (pack,unpack)
import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import           Data.TCache.DefaultPersistence ()


type Ref
  = T.DBRef

instance ToJSON (T.DBRef a) where
  toJSON = toJSON . pack . T.keyObjDBRef

instance (T.IResource a,Typeable a) => FromJSON (T.DBRef a) where
  parseJSON = (T.getDBRef . unpack <$>) . parseJSON

instance JSONSchema (T.DBRef a) where
  schema _ = schema (Proxy :: Proxy Text)

type RefID a
  = Ref (WithID a)

instance T.Indexable (WithID a) where
  key (WithID i _) = show i
  defPath _ = "TCache"

instance (FromJSON a,ToJSON a) => T.Serializable a where
  serialize   = encode
  deserialize = either error id . eitherDecode


data Question
  = Question
    {
      question :: Text
    , answer   :: Answer
    }
  deriving (Generic,Typeable)
instance ToJSON Question where
  toJSON = gtoJson
instance FromJSON Question where
  parseJSON = gparseJson
instance JSONSchema Question where
  schema = gSchema

data Answer
  = Open Text
  | MultipleChoice Text [Text]
  deriving (Generic,Typeable)
instance ToJSON Answer where
  toJSON = gtoJson
instance FromJSON Answer where
  parseJSON = gparseJson
instance JSONSchema Answer where
  schema = gSchema

data Test
  = Test
    {
      name      :: Text
    , questions :: [RefID Question]
    }
  deriving (Generic,Typeable)
instance ToJSON Test where
  toJSON = gtoJson
instance FromJSON Test where
  parseJSON = gparseJson
instance JSONSchema Test where
  schema = gSchema
