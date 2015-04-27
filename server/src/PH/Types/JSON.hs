{-# LANGUAGE FlexibleContexts #-}
module PH.Types.JSON where

import           Common
import qualified Data.TCache.ID as ID
import           PH.Types

import           Data.JSON.Schema (Proxy(Proxy))
import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import           Data.Text        (pack,unpack)
import           Data.Typeable    (typeRep,Proxy(Proxy))


instance ToJSON (T.DBRef a) where
  toJSON = toJSON . pack . T.keyObjDBRef
instance (T.IResource a,Typeable a) => FromJSON (T.DBRef a) where
  parseJSON = (T.getDBRef . unpack <$>) . parseJSON
instance JSONSchema (T.DBRef a) where
  schema _ = schema (Proxy :: Proxy Text)

instance ToJSON Question where
  toJSON = gtoJson
instance FromJSON Question where
  parseJSON = gparseJson
instance JSONSchema Question where
  schema = gSchema

instance ToJSON Answer where
  toJSON = gtoJson
instance FromJSON Answer where
  parseJSON = gparseJson
instance JSONSchema Answer where
  schema = gSchema

instance ToJSON Test where
  toJSON = gtoJson
instance FromJSON Test where
  parseJSON = gparseJson
instance JSONSchema Test where
  schema = gSchema


instance (FromJSON a) => FromJSON (ID.WithID a) where
  parseJSON = gparseJson
instance (ToJSON a) => ToJSON (ID.WithID a) where
  toJSON = gtoJson
instance (JSONSchema a) => JSONSchema (ID.WithID a) where
  schema = gSchema

instance forall a. (Typeable a) => T.Indexable (ID.WithID a) where
  key (ID.WithID i _) = show (typeRep (Proxy :: Proxy a)) ++ "-" ++ show i
  defPath _ = "TCache/"
