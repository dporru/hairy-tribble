{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PH.DB
  (
    initialise
  , finalise
  , run
  , withStore
  , store
  , stm
  
  , flush
  ) where

import           Accounts  (Account,accountStore)
import           Common
import           PH.Types
import           PH.Types.Indices
import           PH.Types.Storage

import qualified Data.TCache             as T
import qualified Data.TCache.Defs        as T


initialise :: T.Persist -> IO ()
initialise store = do
  T.initialise store
  initialiseIndices store

finalise :: T.Persist -> IO ()
finalise store = T.syncCache store

newtype DBM a
  = DBM (ReaderT T.Persist STM a)
  deriving (Functor,Applicative,Monad)


run :: (MonadIO m) => Account -> DBM a -> m a
run account (DBM h) = do
  let store = accountStore account
  let s = runReaderT h store
  liftIO . T.atomicallySync store $ s

stm :: STM a -> DBM a
stm = DBM . lift

store :: DBM T.Persist
store = DBM ask

withStore :: (T.Persist -> STM a) -> DBM a
withStore f = store >>= stm . f

flush :: DBM ()
flush = withStore T.flushAll
