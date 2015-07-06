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

SC.deriveSafeCopy 0 'SC.base ''Question
SC.deriveSafeCopy 0 'SC.base ''Answer
SC.deriveSafeCopy 0 'SC.base ''Test
SC.deriveSafeCopy 0 'SC.base ''Dated
SC.deriveSafeCopy 0 'SC.base ''Labelled
SC.deriveSafeCopy 0 'SC.base ''Dates
SC.deriveSafeCopy 0 'SC.base ''ID.ID
SC.deriveSafeCopy 0 'SC.base ''ID.WithID
instance (T.IResource a,Typeable a) => SC.SafeCopy (T.DBRef a) where
  putCopy = SC.contain . SC.safePut . Text.pack . T.keyObjDBRef
  getCopy = SC.contain $ T.getDBRef . Text.unpack <$> SC.safeGet
