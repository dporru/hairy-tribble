{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PH.DB
  (
    Store
  , Stores
  , initialise
  , finalise
  , run
  , withStore
  , store
  , stm
  
  , flush
  ) where

import           Accounts  (Account(Account))
import           Common
import           PH.Types
import           PH.Types.Indices
import           PH.Types.Storage

import qualified Data.Map         as Map
import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T


type Store
  = T.Persist

type Stores
  = Map.Map Account Store

initialise :: Account -> IO Store
initialise account = do
  store <- accountStore account
  T.initialise store
  initialiseIndices store
  return store
 where
  accountStore :: Account -> IO Store
  accountStore account = T.filePersist $ "data/" ++ accountPath account
  
  accountPath :: Account -> String
  accountPath (Account a) = "account/" ++ a

finalise :: Store -> IO ()
finalise store = T.syncCache store

newtype DBM a
  = DBM (ReaderT Store STM a)
  deriving (Functor,Applicative,Monad)


run :: (MonadIO m) => Store -> DBM a -> m a
run store (DBM h) = do
  let s = runReaderT h store
  liftIO . T.atomicallySync store $ s

stm :: STM a -> DBM a
stm = DBM . lift

store :: DBM Store
store = DBM ask

withStore :: (Store -> STM a) -> DBM a
withStore f = store >>= stm . f

flush :: DBM ()
flush = withStore T.flushAll
