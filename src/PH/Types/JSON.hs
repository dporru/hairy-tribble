{-# LANGUAGE FlexibleContexts #-}
module PH.Types.JSON where

import           Common
import qualified Data.TCache.ID as ID
import           PH.Types

import           Data.Aeson.Types (typeMismatch,Value(String))
import           Data.JSON.Schema (Proxy(Proxy))
import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.Text        as Text
import           Data.Typeable    (typeRep,Proxy(Proxy))


instance ToJSON (T.DBRef a) where
  toJSON = toJSON . Text.pack . T.keyObjDBRef
instance (T.IResource a,Typeable a) => FromJSON (T.DBRef a) where
  parseJSON = (T.getDBRef . Text.unpack <$>) . parseJSON
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


instance (Typeable a,FromJSON a) => FromJSON (ID.ID a) where
  parseJSON s = do
    t :: Text <- parseJSON s
    case Text.split (== '-') t of
      [ts,rest]
        | ts == typeString -> return $ ID.ID rest
      _                    -> typeMismatch "PH.Types.JSON: ID.ID" (String t)
   where
    typeString = Text.pack . show $ typeRep (Proxy :: Proxy a)

instance (Typeable a,ToJSON a) => ToJSON (ID.ID a) where
  toJSON (ID.ID t) = toJSON $ typeString <> "-" <> t
   where
    typeString = Text.pack . show $ typeRep (Proxy :: Proxy a)

instance (JSONSchema a) => JSONSchema (ID.ID a) where
  schema _ = schema (Proxy :: Proxy Text)


instance (Typeable a,FromJSON a) => FromJSON (ID.WithID a) where
  parseJSON = gparseJson
instance (Typeable a,ToJSON a) => ToJSON (ID.WithID a) where
  toJSON = gtoJson
instance (JSONSchema a) => JSONSchema (ID.WithID a) where
  schema = gSchema

instance forall a. (Typeable a) => T.Indexable (ID.WithID a) where
  key (ID.WithID (ID.ID t) _) = show (typeRep (Proxy :: Proxy a)) ++ "-" ++ Text.unpack t
--   defPath _ = "TCache/"
