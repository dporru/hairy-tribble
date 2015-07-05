{-# LANGUAGE FlexibleContexts #-}
module Rest.TCache.ID
  (
    Resource
  , resource
  ) where

import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.TCache.ID   as ID

import qualified Rest             as R
import           Rest (Void)
import qualified Rest.Handler     as R
import qualified Rest.Resource    as R

import           Control.Applicative        ((<$>))
import           Control.Concurrent.STM     (STM)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (ToJSON,FromJSON)
import qualified Data.Map         as Map
import           Data.JSON.Schema           (JSONSchema)
import qualified Data.Set         as Set
import           Data.Typeable              (Typeable)


type Resource o
  = R.Resource IO (ReaderT (ID.Ref o) IO) (ID.Ref o) () Void

resource :: forall o.
  ( T.Indexable (ID.WithID o),T.PersistIndex (ID.WithID o)
  , T.Serializable (ID.WithID o),T.IResource (ID.WithID o)
  , JSONSchema (ID.Ref o),JSONSchema (ID.WithID o),JSONSchema o
  , ToJSON (ID.Ref o),ToJSON (ID.WithID o),ToJSON o,FromJSON o
  , Typeable o
  , T.IResource (Map.Map (ID.ID o) (Set.Set (ID.Ref o)))
  , T.Serializable (Map.Map (ID.ID o) (Set.Set (ID.Ref o)))
  , T.Indexable    (Map.Map (ID.ID o) (Set.Set (ID.Ref o)))
  ) => String -> Resource o
resource name = R.mkResourceReader
  {
    R.name   = name
  , R.schema = R.withListing () $ R.unnamedSingle T.getDBRef
  , R.list   = const list
  , R.get    = Just get
  , R.update = Just update
  , R.create = Just create
  , R.remove = Just remove
  }
 where
  get = R.mkIdHandler R.jsonO $ \ () -> run . ID.maybeDeref
  list = R.mkListing R.jsonO $ \ range -> take (R.count range) . drop (R.offset range)
    <$> run (ID.listWithID :: STM [ID.WithID o])
  create = R.mkInputHandler (R.jsonI . R.jsonO) $ run . (ID.newRef :: o -> STM (ID.Ref o))
  remove = R.mkIdHandler id $ \ () -> run . ID.delete
  update = R.mkIdHandler R.jsonI $ \ (x :: o) i -> do
    maybe (throwError R.NotFound) return =<< run (ID.update i x)
  
  run :: (MonadIO m) => STM a -> m a
  run = liftIO . T.atomicallySync
