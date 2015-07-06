{-# LANGUAGE FlexibleContexts #-}
module Rest.TCache.ID
  (
    Resource
  , resource
  ) where

import           PH.Types (Decorated)
import           PH.Types.Dated
import           PH.Types.Labelled

import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.TCache.ID   as ID

import qualified Rest             as R
import           Rest (Void)
import qualified Rest.Handler     as R
import qualified Rest.Resource    as R

import           Control.Applicative        ((<$>))
import           Control.Concurrent.STM     (STM)
import           Control.Lens               (view,over)
import           Control.Monad.Except       (MonadError,throwError)
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (ToJSON,FromJSON)
import qualified Data.Map         as Map
import           Data.JSON.Schema           (JSONSchema)
import qualified Data.Set         as Set
import           Data.Typeable              (Typeable)


type Resource o
  = R.Resource IO (ReaderT (ID.Ref (Decorated o)) IO) (ID.Ref (Decorated o)) () Void

resource :: forall o.
  ( T.Indexable (ID.WithID o),T.PersistIndex (ID.WithID o)
  , T.Serializable (ID.WithID (Decorated o)),T.IResource (ID.WithID o)
  , JSONSchema (ID.Ref (Decorated o)),JSONSchema (ID.WithID (Decorated o))
  , JSONSchema (Decorated o),JSONSchema (Labelled o),JSONSchema o
  , ToJSON (ID.Ref (Decorated o)),ToJSON (ID.WithID (Decorated o)),ToJSON (Decorated o)
  , FromJSON (Decorated o),FromJSON (Labelled o)
  , Typeable o
  , T.IResource (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Serializable (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Indexable    (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
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
  list = R.mkListing R.jsonO $ \ range ->
    take (R.count range) . drop (R.offset range)
     . filter (not . isDeleted . view withoutLabels . ID.object)
    <$> run (ID.listWithID :: STM [ID.WithID (Decorated o)])
  create = R.mkInputHandler (R.jsonI . R.jsonO) $ \ (o :: Labelled o) -> do
    run . ID.newRef =<< withoutLabels date o
  remove = R.mkIdHandler id $ \ () i -> do
    c <- liftIO getCurrentTime
    found <- run $ ID.update i . over withoutLabels (deleteDated c) =<< ID.deref i
    maybeNotFound found
  update = R.mkIdHandler R.jsonI $ \ (x :: Decorated o) i -> do
    c <- liftIO getCurrentTime
    found <- run (ID.update i . over withoutLabels (updateDated c) $ x)
    maybeNotFound found
  
  run :: (MonadIO m) => STM a -> m a
  run = liftIO . T.atomicallySync

maybeNotFound :: (MonadError (R.Reason e) m) => Maybe () -> m ()
maybeNotFound = maybe (throwError R.NotFound) return
