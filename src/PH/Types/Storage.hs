{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module PH.Types.Storage where

import           Common
import           PH.Types

import qualified Data.SafeCopy      as SC
import qualified Data.Serialize.Get as C
import qualified Data.Serialize.Put as C
import           Data.JSON.Schema (Proxy(Proxy))
import qualified Data.TCache      as T
import qualified Data.Text        as Text
import           Data.Typeable    (typeRep,Proxy(Proxy))
import qualified Data.TCache.Defs   as T
import qualified Data.TCache.ID     as ID
import           System.IO.Unsafe (unsafePerformIO)


instance (SC.SafeCopy a) => T.Serializable a where
  serialize   = C.runPutLazy . SC.safePut
  deserialize = either error id . C.runGetLazy SC.safeGet

-- instance (Typeable a) => T.Indexable (ID.WithID a) where
--   key (ID.WithID (ID.ID t) _) = show (typeRep (Proxy :: Proxy a)) ++ "-" ++ Text.unpack t
--   defPath _ = "TCache/"

SC.deriveSafeCopy 1 'SC.extension ''Question
SC.deriveSafeCopy 0 'SC.base ''Question_v0
SC.deriveSafeCopy 0 'SC.base ''Answer
SC.deriveSafeCopy 1 'SC.extension ''Test
SC.deriveSafeCopy 0 'SC.base ''Test_v0
SC.deriveSafeCopy 0 'SC.base ''Dates
SC.deriveSafeCopy 0 'SC.base ''ID.ID
SC.deriveSafeCopy 0 'SC.base ''ID.WithID
instance (T.IResource a,Typeable a) => SC.SafeCopy (T.DBRef a) where
  putCopy = SC.contain . SC.safePut . Text.pack . T.keyObjDBRef
  getCopy = SC.contain $ T.getDBRef . Text.unpack <$> SC.safeGet

instance SC.Migrate Question where
  type MigrateFrom Question = Question_v0
  migrate (Question_v0 q a) = Question q a (unsafePerformIO newDates)
instance SC.Migrate Test where
  type MigrateFrom Test = Test_v0
  migrate (Test_v0 q a) = Test q a (unsafePerformIO newDates)
